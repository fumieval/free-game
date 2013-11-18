{-# LANGUAGE FlexibleContexts, Rank2Types #-}
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
module Graphics.UI.FreeGame.GUI.GLFW (runGame) where
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
import Graphics.UI.FreeGame.Internal.Raindrop
import Graphics.UI.FreeGame.GUI
import Graphics.UI.FreeGame.Types
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
import Data.Reflection

runGame :: GUIParam -> F GUI a -> IO (Maybe a)
runGame param m = launch param $ runF m (return . Just) runAction

data System = System
    { refTextures :: IORef (IM.IntMap Texture)
    , refFrame :: IORef Int
    , refFPS :: IORef Int
    , theParameter :: GUIParam
    }

runAction :: Given System => GUI (FinalizerT IO (Maybe a)) -> FinalizerT IO (Maybe a)
runAction (LiftUI f) = join $ runUI id f
runAction (EmbedIO m) = join (liftIO m)
runAction (Bracket m) = liftIO $ runFinalizerT $ runF m (return.Just) runAction
        >>= maybe (return Nothing) id
runAction Quit = return Nothing
runAction (Tick cont) = do
    liftIO $ do
        GL.matrixMode $= GL.Projection
        GLFW.swapBuffers
        -- performGC
        t <- GLFW.getTime
        n <- readIORef (refFrame given)
        GLFW.sleep $ fromIntegral n / fromIntegral (_framePerSecond $ theParameter given) - t
        if t >= 1
            then GLFW.resetTime >> writeIORef (refFrame given) 0 >> writeIORef (refFPS given) n
            else writeIORef (refFrame given) (succ n)

    r <- liftIO $ GLFW.windowIsOpen
    if not r then return Nothing else do
        liftIO $ do
            GL.clear [GL.ColorBuffer] 
            GL.loadIdentity
            let BoundingBox wl wt wr wb = fmap realToFrac $ _windowRegion $ theParameter given
            GL.ortho wl wr wb wt 0 (-100)
            GL.matrixMode $= GL.Modelview 0
        cont
runAction (GetFPS cont) = liftIO (readIORef (refFPS given)) >>= cont

type Texture = (GL.TextureObject, Int, Int)

launch :: GUIParam -> (Given System => FinalizerT IO (Maybe a)) -> IO (Maybe a)
launch param m = do
    GLFW.initialize >>= bool (fail "Failed to initialize") (return ())
    pf <- GLFW.openGLProfile
    let V2 ww wh = fmap floor (view _BottomRight (_windowRegion param) - view _TopLeft (_windowRegion param))
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
    GL.shadeModel $= GL.Flat
    GL.textureFunction $= GL.Combine

    let Color r g b a = _clearColor param in GL.clearColor $= GL.Color4 (gf r) (gf g) (gf b) (gf a)

    sys <- System <$> newIORef IM.empty <*> newIORef 0 <*> newIORef 0 <*> pure param
    res <- runFinalizerT $ give sys m

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
    return (tex, width, height)

rot2 :: Floating a => a -> V2 a -> V2 a
rot2 a (V2 x y) = V2 (p * x + q * y) (-q * x + p * y) where
    d = a * (pi / 180) 
    p = cos d
    q = sin d

runUI :: Given System => (V2 Float -> V2 Float) -> GUIBase a -> FinalizerT IO a
runUI _ (FromBitmap bmp@(BitmapData _ (Just h)) r) = do
    m <- liftIO $ readIORef $ refTextures given
    case IM.lookup h m of
        Just t -> liftIO $ drawTexture t
        Nothing -> do
            t <- installTexture bmp
            liftIO $ writeIORef (refTextures given) $ IM.insert h t m
            liftIO $ drawTexture t
            finalizer $ modifyIORef (refTextures given) $ IM.delete h
    return r
runUI _ (FromBitmap bmp@(BitmapData _ Nothing) r) = do
    liftIO $ runFinalizerT $ installTexture bmp >>= liftIO . drawTexture
    return r
runUI f (Translate t@(V2 tx ty) inner) = preservingMatrix' $ do
    liftIO $ GL.translate (GL.Vector3 (gf tx) (gf ty) 0)
    runUI (subtract t . f) inner
runUI f (RotateD theta inner) = preservingMatrix' $ do
    liftIO $ GL.rotate (gf (-theta)) (GL.Vector3 0 0 1)
    runUI (rot2 (-theta) . f) inner
runUI f (Scale s@(V2 sx sy) inner) = preservingMatrix' $ do
    liftIO $ GL.scale (gf sx) (gf sy) 1
    runUI ((/s) . f) inner
runUI _ (FromFinalizer m) = m
runUI f (Colored col inner) = do
    oldColor <- liftIO $ get GL.currentColor
    liftIO $ GL.currentColor $= unsafeCoerce col
    res <- runUI f inner
    liftIO $ GL.currentColor $= oldColor
    return res
runUI _ (Line path r) = do
    liftIO $ GL.renderPrimitive GL.LineStrip $ runVertices path
    return r
runUI _ (Polygon path r) = do
    liftIO $ GL.renderPrimitive GL.Polygon $ runVertices path
    return r
runUI _ (PolygonOutline path r) = do
    liftIO $ GL.renderPrimitive GL.LineLoop $ runVertices path
    return r
runUI _ (Circle r cont) = do
    let s = 2 * pi / r
    liftIO $ GL.renderPrimitive GL.Polygon $ runVertices [V2 (cos t * r) (sin t * r) | t <- [0,s..2 * pi]]
    return cont
runUI _ (CircleOutline r cont) = do
    let s = 2 * pi / r
    liftIO $ GL.renderPrimitive GL.LineLoop $ runVertices [V2 (cos t * r) (sin t * r) | t <- [0,s..2 * pi]]
    return cont
runUI f (Thickness t inner) = do
    oldWidth <- liftIO $ get GL.lineWidth
    liftIO $ GL.lineWidth $= gf t
    res <- runUI f inner
    liftIO $ GL.lineWidth $= oldWidth
    return res
runUI _ (KeyChar ch cont) = cont <$> liftIO (GLFW.keyIsPressed (GLFW.CharKey ch))
runUI _ (KeySpecial x cont) = cont <$> liftIO (GLFW.keyIsPressed (mapSpecialKey x))
runUI _ (MouseButtonL cont) = cont <$> liftIO (GLFW.mouseButtonIsPressed GLFW.MouseButton0)
runUI _ (MouseButtonR cont) = cont <$> liftIO (GLFW.mouseButtonIsPressed GLFW.MouseButton1)
runUI _ (MouseButtonM cont) = cont <$> liftIO (GLFW.mouseButtonIsPressed GLFW.MouseButton2)
runUI f (MousePosition cont) = do
    (x, y) <- liftIO GLFW.getMousePosition
    let pos = f $ V2 (fromIntegral x) (fromIntegral y)
    return $ cont pos
runUI _ (MouseWheel cont) = cont <$> liftIO GLFW.getMouseWheel

runVertices :: MonadIO m => [V2 Float] -> m ()
runVertices = liftIO . mapM_ (GL.vertex . mkVertex2)

preservingMatrix' :: MonadIO m => m a -> m a
preservingMatrix' m = do
    liftIO $ glPushMatrix
    r <- m
    liftIO $ glPopMatrix
    return r

drawTexture :: Texture -> IO ()
drawTexture (tex, width, height) = do
    let (w, h) = (fromIntegral width / 2, fromIntegral height / 2) :: (GL.GLfloat, GL.GLfloat)
    GL.texture GL.Texture2D $= GL.Enabled
    GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)
    GL.textureBinding GL.Texture2D $= Just tex
    GL.renderPrimitive GL.Polygon $ do
        GL.texCoord $ GL.TexCoord2 (0 :: GL.GLfloat) 0
        GL.vertex $ GL.Vertex2 (-w) (-h)
        GL.texCoord $ GL.TexCoord2 (1 :: GL.GLfloat) 0
        GL.vertex $ GL.Vertex2 w (-h)
        GL.texCoord $ GL.TexCoord2 (1 :: GL.GLfloat) 1
        GL.vertex $ GL.Vertex2 w h
        GL.texCoord $ GL.TexCoord2 (0 :: GL.GLfloat) 1
        GL.vertex $ GL.Vertex2 (-w) h
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

mkVertex2 :: V2 Float -> GL.Vertex2 GL.GLfloat
mkVertex2 = unsafeCoerce
{-# INLINE mkVertex2 #-}

gf :: Float -> GL.GLfloat
{-# INLINE gf #-}
gf x = unsafeCoerce x

gsizei :: Int -> GL.GLsizei
{-# INLINE gsizei #-}
gsizei x = unsafeCoerce x