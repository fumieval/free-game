{-# LANGUAGE ImplicitParams, BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.FreeGame.GUI.GLFW
-- Copyright   :  (C) 2013 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Graphics.UI.FreeGame.GUI.GLFW (runGame
    -- * Implementation details
    , Texture
    , installTexture
    , drawTexture
    , drawTextureAt) where
import Control.Applicative
import Control.Monad
import Control.Monad.Free.Church
import Control.Monad.IO.Class
import Data.IORef
import Data.Color
import Data.Char
import Foreign.ForeignPtr
import Graphics.UI.FreeGame.Base
import Graphics.UI.FreeGame.Data.Bitmap
import Graphics.UI.FreeGame.Internal.Finalizer
import Graphics.UI.FreeGame.GUI
import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility
import qualified Data.Array.Repa.Repr.ForeignPtr as RF
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.GL.StateVar
import qualified Data.IntMap as IM
import qualified Graphics.Rendering.OpenGL.GL as GL
import System.Mem
import Unsafe.Coerce
import Control.Bool
import Linear

data System = System
    { refFrameCounter :: IORef Int
    , refFPS :: IORef Int
    , refTextures :: IORef Texture
    }

data Window = Window
    { _windowBoundingBox :: BoundingBox Float
    , _windowMain :: F GUI ()
    , _windowTitle :: String
    , _windowResizable :: Bool
    }

runGame :: GUIParam -> F GUI a -> IO (Maybe a)
runGame param m = launch param $ unUI m runGUI tickIO (return . Just)

runGUI :: Given System => GUI (FinalizerT IO a) -> FinalizerT IO a
runGUI (FromBitmap bmp@(BitmapData _ (Just h)) cont) = do
    m <- liftIO $ readIORef (refTextures given)
    case IM.lookup h m of
        Just t -> liftIO $ drawTexture t
        Nothing -> do
            t <- installTexture bmp
            liftIO $ writeIORef (refTextures given) $ IM.insert h t m
            liftIO $ drawTexture t
            finalizer $ modifyIORef (refTextures given) $ IM.delete h
    cont
runGUI (Translate (V2 tx ty) m) = preservingMatrix' $ do
    liftIO $ GL.translate (GL.Vector3 (gf tx) (gf ty) 0)
    runGUI m
runGUI (FromBitmap bmp@(BitmapData _ Nothing) cont) = do
    liftIO $ runFinalizerT $ installTexture bmp >>= liftIO . drawTexture
    cont
runGUI (RotateD theta f) = preservingMatrix' $ do
    liftIO $ GL.rotate (gf (-theta)) (GL.Vector3 0 0 1)
    runGUI f
runGUI (Scale (V2 sx sy) f) = preservingMatrix' $ do
    liftIO $ GL.scale (gf sx) (gf sy) 1
    runGUI f
runGUI (Colored col m) = do
    oldColor <- liftIO $ get GL.currentColor
    liftIO $ GL.currentColor $= unsafeCoerce col
    r <- runGUI m
    liftIO $ GL.currentColor $= oldColor
    return r
runGUI (CharKey ch cont) = GLFW.getKey (mapCharKey ch) >>= cont
runGUI (SpecialKey k cont) = GLFW.getKey (mapSpecialKey k) >>= cont
runGUI (MouseButtonL cont) = GLFW.mouseButtonIsPressed GLFW.MouseButton0 >>= cont
runGUI (MouseButtonR cont = GLFW.mouseButtonIsPressed GLFW.MouseButton1 >>= cont
runGUI (MouseButtonM cont) = GLFW.mouseButtonIsPressed GLFW.MouseButton2 >>= cont
runGUI (MouseWheel cont) = GLFW.getMouseWheel >>= cont
runGUI (MousePosition cont) = do
    (x, y) <- GLFW.getMousePosition
    -- It should pass the relative position
    V2 (fromIntegral x) (fromIntegral y) >>= cont
runGUI (Circle r) = do
        let s = 2 * pi / 64
        GL.renderPrimitive GL.Polygon $ runVertices [V2 (cos t * r) (sin t * r) | t <- [0,s..2 * pi]]
runGUI (CircleOutline r) = do
        let s = 2 * pi / 64 * sc
        GL.renderPrimitive GL.LineLoop $ runVertices [V2 (cos t * r) (sin t * r) | t <- [0,s..2 * pi]]
runGUI (Polygon path) = GL.renderPrimitive GL.Polygon $ runVertices path
runGUI (PolygonOutline path) = GL.renderPrimitive GL.LineLoop $ runVertices path
runGUI (Line path) = GL.renderPrimitive GL.LineStrip $ runVertices path
runGUI (Thickness t m) = do
    oldWidth <- get GL.lineWidth
    GL.lineWidth $= gf t
    r <- m
    GL.lineWidth $= oldWidth
    return r

tickIO :: Given System => IO ()
tickIO = do
    GL.matrixMode $= GL.Projection
    GLFW.swapBuffers
    performGC
    t <- GLFW.getTime
    n <- readIORef given
    GLFW.sleep $ fromIntegral n / fromIntegral (_framePerSecond param) - t
    if t > 1
        then GLFW.setTime 0 >> writeIORef given 0
        else writeIORef given (succ n)

    GL.clear [GL.ColorBuffer] 
    GL.loadIdentity
    GL.scale (gf 1) (-1) 1
    let V2 ox oy = _windowOrigin param
        V2 ww wh = _windowSize param
        windowL = realToFrac ox
        windowR = realToFrac ox + fromIntegral ww
        windowT = realToFrac oy
        windowB = realToFrac oy + fromIntegral wh
    GL.ortho windowL windowR windowT windowB 0 (-100)
    GL.matrixMode $= GL.Modelview 0

type Texture = (GL.TextureObject, Double, Double)

launch :: GUIParam -> FinalizerT IO (Maybe a) -> IO (Maybe a)
launch param m = do
    () <- unlessM GLFW.init (fail "Failed to initialize")
    pf <- GLFW.openGLProfile
    let V2 ww wh = _windowSize param
    createWindow  GLFW.openWindow $ GLFW.defaultDisplayOptions {
        GLFW.displayOptions_width = ww
        ,GLFW.displayOptions_height = wh
        ,GLFW.displayOptions_displayMode = if _windowed param then GLFW.Window else GLFW.Fullscreen
        ,GLFW.displayOptions_windowIsResizable = False
        ,GLFW.displayOptions_openGLProfile = pf
        }

    GLFW.setWindowTitle $ _windowTitle param
    GL.lineSmooth $= GL.Enabled
    GL.blend      $= GL.Enabled
    GL.blendFunc  $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    GL.shadeModel $= GL.Flat
    GL.textureFunction $= GL.Combine

    let Color r g b a = _clearColor param in GL.clearColor $= GL.Color4 (gf r) (gf g) (gf b) (gf a)

    res <- runFinalizerT m

    GLFW.closeWindow
    GLFW.terminate
    return res

installTexture :: Bitmap -> FinalizerT IO Texture
installTexture bmp@(BitmapData ar _) = do
    [tex] <- liftIO $ GL.genObjectNames 1
    liftIO $ GL.textureBinding GL.Texture2D GL.$= Just tex
    let (width, height) = bitmapSize bmp
    let siz = GL.TextureSize2D (gsizei width) (gsizei height)
    liftIO $ withForeignPtr (RF.toForeignPtr ar)
        $ GL.texImage2D Nothing GL.NoProxy 0 GL.RGBA8 siz 0
        . GL.PixelData GL.RGBA GL.UnsignedInt8888
    finalizer $ GL.deleteObjectNames [tex]
    return (tex, fromIntegral width / 2, fromIntegral height / 2)

fromKeyState :: KeyState -> Bool
fromKeyState KeyState'Pressed = True
fromKeyState KeyState'Released = False
fromKeyState KeyState'Repeating = True

runVertices :: MonadIO m => [V2 Double] -> m ()
runVertices = liftIO . mapM_ (GL.vertex . mkVertex2)
{-# INLINE runVertices #-}

preservingMatrix' :: MonadIO m => m a -> m a
preservingMatrix' m = do
    liftIO $ glPushMatrix
    r <- m
    liftIO $ glPopMatrix
    return r

drawTexture :: Texture -> IO ()
drawTexture (tex, w, h) = drawTextureAt tex (V2 (-w) (-h)) (V2 w (-h)) (V2 w h) (V2 (-w) h)
{-# INLINE drawTexture #-}

drawTextureAt :: GL.TextureObject -> V2 Double -> V2 Double -> V2 Double -> V2 Double -> IO ()
drawTextureAt tex a b c d = do
    GL.texture GL.Texture2D $= GL.Enabled
    GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)
    GL.textureBinding GL.Texture2D $= Just tex
    GL.unsafeRenderPrimitive GL.TriangleStrip $ do
        GL.texCoord $ GL.TexCoord2 (0 :: GL.GLdouble) 0
        GL.vertex $ mkVertex2 a
        GL.texCoord $ GL.TexCoord2 (1 :: GL.GLdouble) 0
        GL.vertex $ mkVertex2 b
        GL.texCoord $ GL.TexCoord2 (0 :: GL.GLdouble) 1
        GL.vertex $ mkVertex2 d
        GL.texCoord $ GL.TexCoord2 (1 :: GL.GLdouble) 1
        GL.vertex $ mkVertex2 c
    GL.texture GL.Texture2D $= GL.Disabled

mkVertex2 :: V2 Double -> GL.Vertex2 GL.GLdouble
{-# INLINE mkVertex2 #-}
mkVertex2 = unsafeCoerce

gf :: Float -> GL.GLfloat
{-# INLINE gf #-}
gf = unsafeCoerce

gd :: Double -> GL.GLdouble
{-# INLINE gd #-}
gd = unsafeCoerce

gsizei :: Int -> GL.GLsizei
{-# INLINE gsizei #-}
gsizei = unsafeCoerce
