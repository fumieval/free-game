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

runGame :: GUIParam -> F GUI a -> IO (Maybe a)
runGame param m = launch param $ \r s -> runF m (return . Just) (runAction param r s)

runAction :: GUIParam
    -> IORef (IM.IntMap Texture)
    -> IORef Int
    -> GUI (FinalizerT IO (Maybe a)) -> FinalizerT IO (Maybe a)
runAction param refTextures refFrame _f = case _f of
    LiftUI (Draw pic) -> let ?refTextures = refTextures in join $ runPicture 1 pic
    EmbedIO m -> join (liftIO m)
    Bracket m -> liftIO (runFinalizerT $ runF m (return.Just) (runAction param refTextures refFrame))
        >>= maybe (return Nothing) id
    LiftUI (Input i) -> join $ liftIO $ runInput i
    Quit -> return Nothing
    Tick cont -> do
        liftIO $ do
            GL.matrixMode $= GL.Projection
            GLFW.swapBuffers
            performGC
            t <- GLFW.getTime
            n <- readIORef refFrame
            GLFW.sleep $ fromIntegral n / fromIntegral (_framePerSecond param) - t
            if t > 1
                then GLFW.resetTime >> writeIORef refFrame 0
                else writeIORef refFrame (succ n)

        r <- liftIO $ GLFW.windowIsOpen
        if not r then return Nothing else do
            liftIO $ do
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
            cont

type Texture = (GL.TextureObject, Double, Double)

launch :: GUIParam -> (IORef (IM.IntMap Texture) -> IORef Int -> FinalizerT IO (Maybe a)) -> IO (Maybe a)
launch param m = do
    GLFW.initialize >>= bool (fail "Failed to initialize") (return ())
    pf <- GLFW.openGLProfile
    let V2 ww wh = _windowSize param
    (>>=bool (fail "Failed to initialize") (return ())) $ GLFW.openWindow $ GLFW.defaultDisplayOptions {
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
    GL.shadeModel $= GL.Smooth
    GL.textureFunction $= GL.Combine

    let Color r g b a = _clearColor param in GL.clearColor $= GL.Color4 (gf r) (gf g) (gf b) (gf a)

    res <- runFinalizerT $ join $ m <$> liftIO (newIORef IM.empty) <*> liftIO (newIORef 0)

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

runInput :: GUIInput a -> IO a
runInput (ICharKey ch cont) = cont <$> GLFW.keyIsPressed (GLFW.CharKey ch)
runInput (ISpecialKey x cont) = cont <$> GLFW.keyIsPressed (mapSpecialKey x)
runInput (IMouseButtonL cont) = cont <$> GLFW.mouseButtonIsPressed GLFW.MouseButton0
runInput (IMouseButtonR cont) = cont <$> GLFW.mouseButtonIsPressed GLFW.MouseButton1
runInput (IMouseButtonM cont) = cont <$> GLFW.mouseButtonIsPressed GLFW.MouseButton2
runInput (IMousePosition cont) = do
    (x, y) <- GLFW.getMousePosition
    return $ cont $ V2 (fromIntegral x) (fromIntegral y)
runInput (IMouseWheel cont) = cont <$> GLFW.getMouseWheel

runPicture :: (?refTextures :: IORef (IM.IntMap Texture)) => Double -> Picture a -> FinalizerT IO a
runPicture _ (LiftBitmap bmp@(BitmapData _ (Just h)) r) = do
    m <- liftIO $ readIORef ?refTextures
    case IM.lookup h m of
        Just t -> liftIO $ drawTexture t
        Nothing -> do
            t <- installTexture bmp
            liftIO $ writeIORef ?refTextures $ IM.insert h t m
            liftIO $ drawTexture t
            finalizer $ modifyIORef ?refTextures $ IM.delete h
    return r
runPicture _ (LiftBitmap bmp@(BitmapData _ Nothing) r) = do
    liftIO $ runFinalizerT $ installTexture bmp >>= liftIO . drawTexture
    return r
runPicture sc (Translate (V2 tx ty) cont) = preservingMatrix' $ do
    liftIO $ GL.translate (GL.Vector3 (gd tx) (gd ty) 0)
    runPicture sc cont
runPicture sc (RotateD theta cont) = preservingMatrix' $ do
    liftIO $ GL.rotate (gd (-theta)) (GL.Vector3 0 0 1)
    runPicture sc cont
runPicture sc (Scale (V2 sx sy) cont) = preservingMatrix' $ do
    liftIO $ GL.scale (gd sx) (gd sy) 1
    runPicture (sc * max sx sy) cont
runPicture _ (PictureWithFinalizer m) = m
runPicture sc (Colored col cont) = do
    oldColor <- liftIO $ get GL.currentColor
    liftIO $ GL.currentColor $= unsafeCoerce col
    res <- runPicture sc cont
    liftIO $ GL.currentColor $= oldColor
    return res
runPicture _ (Line path a) = do
    liftIO $ GL.renderPrimitive GL.LineStrip $ runVertices path
    return a
runPicture _ (Polygon path a) = do
    liftIO $ GL.renderPrimitive GL.Polygon $ runVertices path
    return a
runPicture _ (PolygonOutline path a) = do
    liftIO $ GL.renderPrimitive GL.LineLoop $ runVertices path
    return a
runPicture sc (Circle r a) = do
    let s = 2 * pi / 64 * sc
    liftIO $ GL.renderPrimitive GL.Polygon $ runVertices [V2 (cos t * r) (sin t * r) | t <- [0,s..2 * pi]]
    return a
runPicture sc (CircleOutline r a) = do
    let s = 2 * pi / 64 * sc
    liftIO $ GL.renderPrimitive GL.LineLoop $ runVertices [V2 (cos t * r) (sin t * r) | t <- [0,s..2 * pi]]
    return a
runPicture sc (Thickness t cont) = do
    oldWidth <- liftIO $ get GL.lineWidth
    liftIO $ GL.lineWidth $= gf t
    res <- runPicture sc cont
    liftIO $ GL.lineWidth $= oldWidth
    return res

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
    GL.renderPrimitive GL.Polygon $ do
        GL.texCoord $ GL.TexCoord2 (0 :: GL.GLfloat) 0
        GL.vertex $ mkVertex2 a
        GL.texCoord $ GL.TexCoord2 (1 :: GL.GLfloat) 0
        GL.vertex $ mkVertex2 b
        GL.texCoord $ GL.TexCoord2 (1 :: GL.GLfloat) 1
        GL.vertex $ mkVertex2 c
        GL.texCoord $ GL.TexCoord2 (0 :: GL.GLfloat) 1
        GL.vertex $ mkVertex2 d
    GL.texture GL.Texture2D $= GL.Disabled

mapSpecialKey :: SpecialKey -> GLFW.Key
mapSpecialKey KeySpace = GLFW.KeySpace
mapSpecialKey KeyEsc = GLFW.KeyEsc
mapSpecialKey KeyLeftShift = GLFW.KeyLeftShift
mapSpecialKey KeyRightShift = GLFW.KeyRightShift
mapSpecialKey KeyLeftControl = GLFW.KeyLeftCtrl
mapSpecialKey KeyRightControl = GLFW.KeyRightCtrl
mapSpecialKey KeyUp = GLFW.KeyUp
mapSpecialKey KeyDown = GLFW.KeyDown
mapSpecialKey KeyLeft = GLFW.KeyLeft
mapSpecialKey KeyRight = GLFW.KeyRight
mapSpecialKey KeyTab = GLFW.KeyTab
mapSpecialKey KeyEnter = GLFW.KeyEnter
mapSpecialKey KeyBackspace = GLFW.KeyBackspace
mapSpecialKey KeyInsert = GLFW.KeyInsert
mapSpecialKey KeyDelete = GLFW.KeyDel
mapSpecialKey KeyPageUp = GLFW.KeyPageup
mapSpecialKey KeyPageDown = GLFW.KeyPagedown
mapSpecialKey KeyHome = GLFW.KeyHome
mapSpecialKey KeyEnd = GLFW.KeyEnd
mapSpecialKey KeyF1 = GLFW.KeyF1
mapSpecialKey KeyF2 = GLFW.KeyF2
mapSpecialKey KeyF3 = GLFW.KeyF3
mapSpecialKey KeyF4 = GLFW.KeyF4
mapSpecialKey KeyF5 = GLFW.KeyF5
mapSpecialKey KeyF6 = GLFW.KeyF6
mapSpecialKey KeyF7 = GLFW.KeyF7
mapSpecialKey KeyF8 = GLFW.KeyF8
mapSpecialKey KeyF9 = GLFW.KeyF9
mapSpecialKey KeyF10 = GLFW.KeyF10
mapSpecialKey KeyF11 = GLFW.KeyF11
mapSpecialKey KeyF12 = GLFW.KeyF12
mapSpecialKey KeyPad0 = GLFW.KeyPad0
mapSpecialKey KeyPad1 = GLFW.KeyPad1
mapSpecialKey KeyPad2 = GLFW.KeyPad2
mapSpecialKey KeyPad3 = GLFW.KeyPad3
mapSpecialKey KeyPad4 = GLFW.KeyPad4
mapSpecialKey KeyPad5 = GLFW.KeyPad5
mapSpecialKey KeyPad6 = GLFW.KeyPad6
mapSpecialKey KeyPad7 = GLFW.KeyPad7
mapSpecialKey KeyPad8 = GLFW.KeyPad8
mapSpecialKey KeyPad9 = GLFW.KeyPad9
mapSpecialKey KeyPadDivide = GLFW.KeyPadDivide
mapSpecialKey KeyPadMultiply = GLFW.KeyPadMultiply
mapSpecialKey KeyPadSubtract = GLFW.KeyPadSubtract
mapSpecialKey KeyPadAdd = GLFW.KeyPadAdd
mapSpecialKey KeyPadDecimal = GLFW.KeyPadDecimal
mapSpecialKey KeyPadEqual = GLFW.KeyPadEqual
mapSpecialKey KeyPadEnter = GLFW.KeyPadEnter

mkVertex2 :: V2 Double -> GL.Vertex2 GL.GLdouble
mkVertex2 = unsafeCoerce
{-# INLINE mkVertex2 #-}

gf :: Float -> GL.GLfloat
{-# INLINE gf #-}
gf x = unsafeCoerce x

gd :: Double -> GL.GLdouble
{-# INLINE gd #-}
gd x = unsafeCoerce x

gsizei :: Int -> GL.GLsizei
{-# INLINE gsizei #-}
gsizei x = unsafeCoerce x