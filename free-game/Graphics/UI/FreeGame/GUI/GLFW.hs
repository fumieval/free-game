-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.FreeGame.Backends.GLFW
-- Copyright   :  (C) 2013 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinsohita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
{-# LANGUAGE ImplicitParams, ScopedTypeVariables, Rank2Types, FlexibleContexts #-}
module Graphics.UI.FreeGame.GUI.GLFW (runGame) where
import Control.Applicative
import Control.Applicative.Free as Ap
import Control.MonadPlus.Free as MonadPlus
import Control.Monad
import Control.Monad.Free.Church
import Control.Monad.IO.Class
import Data.IORef
import Data.Monoid
import Foreign.ForeignPtr
import Graphics.UI.FreeGame.Base
import Graphics.UI.FreeGame.Types
import Graphics.UI.FreeGame.Data.Bitmap
import Graphics.UI.FreeGame.Data.Color
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
import Linear

runGame :: GUIParam -> F GUI a -> IO (Maybe a)
runGame param m = launch param $ \r s -> runF m (return . Just) (runAction param r s)


runAction :: GUIParam
    -> IORef (IM.IntMap Texture)
    -> IORef Int
    -> GUI (FinalizerT IO (Maybe a)) -> FinalizerT IO (Maybe a)
runAction param refTextures refFrame _f = case _f of
    LiftUI (Draw pic) -> let ?refTextures = refTextures in join $ runPicture pic
    EmbedIO m -> join (liftIO m)
    Bracket m -> liftIO (runFinalizerT $ runF m (return.Just) (runAction param refTextures refFrame)) >>= maybe (return Nothing) id
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

data Texture = Texture GL.TextureObject !Int !Int

bool :: a -> a -> Bool -> a
bool r _ False = r
bool _ r True = r

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
    GL.texture GL.Texture2D $= GL.Enabled
    GL.textureFunction $= GL.Combine

    let Color r g b a = _clearColor param in GL.clearColor $= GL.Color4 (gf r) (gf g) (gf b) (gf a)

    res <- runFinalizerT $ join $ m <$> liftIO (newIORef IM.empty) <*> liftIO (newIORef 0)

    GLFW.closeWindow
    GLFW.terminate
    return res

installTexture :: Bitmap -> FinalizerT IO Texture
installTexture bmp = do
    [tex] <- liftIO $ GL.genObjectNames 1
    liftIO $ GL.textureBinding GL.Texture2D GL.$= Just tex
    let (width, height) = bitmapSize bmp
    let siz = GL.TextureSize2D (gsizei width) (gsizei height)
    liftIO $ withForeignPtr (RF.toForeignPtr $ bitmapData bmp)
        $ GL.texImage2D Nothing GL.NoProxy 0 GL.RGBA8 siz 0
        . GL.PixelData GL.RGBA GL.UnsignedInt8888
    finalizer $ GL.deleteObjectNames [tex]
    return $ Texture tex width height

runInput :: Ap GUIInput a -> IO a
runInput (Ap.Pure a) = pure a
runInput (Ap.Ap v af) = (runInput af <*>) $ case v of
    ICharKey ch cont -> cont <$> GLFW.keyIsPressed (GLFW.CharKey ch)
    ISpecialKey x cont -> cont <$> GLFW.keyIsPressed (mapSpecialKey x)
    IMouseButtonL cont -> cont <$> GLFW.mouseButtonIsPressed GLFW.MouseButton0
    IMouseButtonR cont -> cont <$> GLFW.mouseButtonIsPressed GLFW.MouseButton1
    IMouseButtonM cont -> cont <$> GLFW.mouseButtonIsPressed GLFW.MouseButton2
    IMousePosition cont -> do
        (x, y) <- GLFW.getMousePosition
        return $ cont $ V2 (fromIntegral x) (fromIntegral y)
    IMouseWheel cont -> cont <$> GLFW.getMouseWheel

runPicture :: (?refTextures :: IORef (IM.IntMap Texture)) => Picture a -> FinalizerT IO a
runPicture (LiftBitmap bmp@(BitmapData _ (Just h)) r) = do
    m <- liftIO $ readIORef ?refTextures
    case IM.lookup h m of
        Just t -> liftIO $ drawTexture t
        Nothing -> do
            t <- installTexture bmp
            liftIO $ writeIORef ?refTextures $ IM.insert h t m
            liftIO $ drawTexture t
            finalizer $ modifyIORef ?refTextures $ IM.delete h
    return r
runPicture (LiftBitmap bmp@(BitmapData _ Nothing) r) = do
    liftIO $ runFinalizerT $ installTexture bmp >>= liftIO . drawTexture
    return r
runPicture (Rotate theta cont) = preservingMatrix' $ do
    liftIO $ GL.rotate (gf (-theta)) (GL.Vector3 0 0 1)
    runPicture cont
runPicture (Scale (V2 sx sy) cont) = preservingMatrix' $ do
    liftIO $ GL.scale (gf sx) (gf sy) 1
    runPicture cont
runPicture (Translate (V2 tx ty) cont) = preservingMatrix' $ do
    liftIO $ GL.translate (GL.Vector3 (gf tx) (gf ty) 0)
    runPicture cont
runPicture (PictureWithFinalizer m) = m
runPicture (Colored (Color r g b a) cont) = do
    oldColor <- liftIO $ get GL.currentColor
    liftIO $ GL.currentColor $= GL.Color4 (gf r) (gf g) (gf b) (gf a)
    r <- runPicture cont
    liftIO $ GL.currentColor $= oldColor
    return r

preservingMatrix' :: MonadIO m => m a -> m a
preservingMatrix' m = do
    liftIO $ glPushMatrix
    r <- m
    liftIO $ glPopMatrix
    return r

gf :: Float -> GL.GLfloat
{-# INLINE gf #-}
gf x = unsafeCoerce x

gsizei :: Int -> GL.GLsizei
{-# INLINE gsizei #-}
gsizei x = unsafeCoerce x

drawTexture :: Texture -> IO ()
drawTexture (Texture tex width height) = do
    let (w, h) = (fromIntegral width / 2, fromIntegral height / 2)
    GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)
    GL.textureBinding GL.Texture2D $= Just tex
    GL.renderPrimitive GL.Polygon $ zipWithM_
        (\(pX, pY) (tX, tY) -> do
            GL.texCoord $ GL.TexCoord2 (gf tX) (gf tY)
            GL.vertex $ GL.Vertex2 (gf pX) (gf pY))
        [(-w, -h), (w, -h), (w, h), (-w, h)]
        [(0,0), (1.0,0), (1.0,1.0), (0,1.0)]

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
