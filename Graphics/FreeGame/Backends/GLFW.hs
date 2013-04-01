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
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
module Graphics.FreeGame.Backends.GLFW (runGame, runGame') where
import Control.Applicative
import Control.Monad
import Control.Monad.Free
import Control.Monad.Free.Church
import Control.Monad.IO.Class
import Data.IORef
import Foreign.ForeignPtr
import Graphics.FreeGame.Base
import Graphics.FreeGame.Data.Bitmap
import Graphics.FreeGame.Internal.Finalizer
import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.GL.StateVar
import qualified Data.Array.Repa.Repr.ForeignPtr as RF
import qualified Data.IntMap as IM
import qualified Graphics.FreeGame.Input as I
import qualified Graphics.Rendering.OpenGL.GL as GL
import System.Mem
import Unsafe.Coerce

runGame :: GameParam -> Game a -> IO (Maybe a)
runGame param m = launch param $ \r s -> runFreeGame param r s m

runGame' :: GameParam -> (forall m. MonadFree GameAction m => m a) -> IO (Maybe a)
runGame' param m = launch param $ \r s -> runF m (return . Just) (runAction param r s)

runFreeGame :: GameParam -> IORef (IM.IntMap Texture) -> IORef Int -> Free GameAction a -> FinalizerT IO (Maybe a)
runFreeGame p r s = go where
    go (Free f) = runAction p r s $ go <$> f
    go (Pure a) = return $ Just a

runAction :: GameParam
    -> IORef (IM.IntMap Texture)
    -> IORef Int
    -> GameAction (FinalizerT IO (Maybe a)) -> FinalizerT IO (Maybe a)
runAction param refTextures refFrame _f = case _f of
    DrawPicture pic cont -> let ?refTextures = refTextures in drawPic pic >> cont
    EmbedIO m -> join (liftIO m)
    Bracket m -> liftIO (runFinalizerT $ runFreeGame param refTextures refFrame m) >>= maybe (return Nothing) id
    Tick cont -> do
        liftIO $ do
            GL.matrixMode $= GL.Projection
            swapBuffers
            t <- getTime
            n <- readIORef refFrame
            sleep (fromIntegral n / fromIntegral (framePerSecond param) - t)
            if t > 1
                then resetTime >> writeIORef refFrame 0
                else writeIORef refFrame (succ n)

        r <- liftIO $ windowIsOpen
        if not r then return Nothing else do
            liftIO $ do
                GL.clear [GL.ColorBuffer] 
                performGC
                GL.loadIdentity
                GL.scale (gf 1) (-1) 1
                let Vec2 ox oy = windowOrigin param
                    windowL = realToFrac ox
                    windowR = realToFrac ox + fromIntegral (fst $ windowSize param)
                    windowT = realToFrac oy
                    windowB = realToFrac oy + fromIntegral (snd $ windowSize param)
                GL.ortho windowL windowR windowT windowB 0 (-100)
                GL.matrixMode $= GL.Modelview 0
            cont
    GetButtonState key fcont -> liftIO (either keyIsPressed mouseButtonIsPressed (mapKey key)) >>= fcont
    GetMousePosition fcont -> do
        (x, y) <- liftIO $ GLFW.getMousePosition
        fcont $ Vec2 (fromIntegral x) (fromIntegral y)
    GetMouseWheel fcont -> liftIO GLFW.getMouseWheel >>= fcont
    GetGameParam fcont -> do -- There may be a better way
        dim <- liftIO GLFW.getWindowDimensions
        fcont $ param { windowSize = dim }
    QuitGame -> return Nothing

data Texture = Texture GL.TextureObject Int Int

launch :: GameParam -> (IORef (IM.IntMap Texture) -> IORef Int -> FinalizerT IO (Maybe a)) -> IO (Maybe a)
launch param m = do
    True <- initialize
    pf <- openGLProfile
    True <- openWindow $ defaultDisplayOptions {
        displayOptions_width = fromIntegral $ fst $ windowSize param
        ,displayOptions_height = fromIntegral $ snd $ windowSize param
        ,displayOptions_displayMode = if windowed param then Window else Fullscreen
        ,displayOptions_windowIsResizable = False
        ,displayOptions_openGLProfile = pf
        }
    
    if cursorVisible param
        then enableMouseCursor 
        else disableMouseCursor

    setWindowTitle $ windowTitle param
    GL.lineSmooth $= GL.Enabled
    GL.blend      $= GL.Enabled
    GL.blendFunc  $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    GL.shadeModel $= GL.Smooth
    GL.texture GL.Texture2D $= GL.Enabled
    GL.textureFunction $= GL.Combine

    let Color r g b a = clearColor param in GL.clearColor $= GL.Color4 (gf r) (gf g) (gf b) (gf a)

    ref <- newIORef IM.empty
    ref' <- newIORef 0
    r <- runFinalizerT $ m ref ref'

    closeWindow
    terminate
    return r

installTexture :: Bitmap -> FinalizerT IO Texture
installTexture bmp = do
    [tex] <- liftIO $ GL.genObjectNames 1
    liftIO $ GL.textureBinding GL.Texture2D GL.$= Just tex

    let (width, height) = bitmapSize bmp
    liftIO $ withForeignPtr (RF.toForeignPtr $ bitmapData bmp)
        $ GL.texImage2D Nothing GL.NoProxy 0 GL.RGBA8 (GL.TextureSize2D (gsizei width) (gsizei height)) 0
        . GL.PixelData GL.RGBA GL.UnsignedInt8888
    finalizer $ GL.deleteObjectNames [tex]
    return $ Texture tex width height

drawTexture :: Texture -> IO ()
drawTexture (Texture tex width height) = do
    let (w, h) = (fromIntegral width / 2, fromIntegral height / 2)
    GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)
    GL.textureBinding GL.Texture2D $= Just tex
    GL.renderPrimitive GL.Polygon $ zipWithM_
        (\(pX, pY) (tX, tY) -> do
            GL.texCoord $ GL.TexCoord2 (gf tX) (gf tY)
            GL.vertex $ GL.Vertex2   (gf pX) (gf pY))
        [(-w, -h), (w, -h), (w, h), (-w, h)]
        [(0,0), (1.0,0), (1.0,1.0), (0,1.0)]

drawPic :: (?refTextures :: IORef (IM.IntMap Texture)) => Picture -> FinalizerT IO ()
drawPic (Bitmap bmp) = case bitmapHash bmp of
    Just h -> do
        m <- liftIO $ readIORef ?refTextures
        case IM.lookup h m of
            Just t -> liftIO $ drawTexture t
            Nothing -> do
                t <- installTexture bmp
                liftIO $ writeIORef ?refTextures $ IM.insert h t m
                liftIO $ drawTexture t
                finalizer $ modifyIORef ?refTextures $ IM.delete h 
    Nothing -> liftIO $ runFinalizerT $ installTexture bmp >>= liftIO . drawTexture

drawPic (BitmapPicture bmp) = drawPic (Bitmap bmp)

drawPic (Rotate theta p) = preservingMatrix' $ do
    liftIO $ GL.rotate (gf (-theta)) (GL.Vector3 0 0 1)
    drawPic p

drawPic (Scale (Vec2 sx sy) p) = preservingMatrix' $ do
    liftIO $ GL.scale (gf sx) (gf sy) 1
    drawPic p

drawPic (Translate (Vec2 tx ty) p) = preservingMatrix' $ do
    liftIO $ GL.translate (GL.Vector3 (gf tx) (gf ty) 0)
    drawPic p

drawPic (Pictures ps) = mapM_ drawPic ps

drawPic (PictureWithFinalizer m) = m >>= drawPic

drawPic (Colored (Color r g b a) pic) = do
    oldColor <- liftIO $ get GL.currentColor
    liftIO $ GL.currentColor $= GL.Color4 (gf r) (gf g) (gf b) (gf a)
    drawPic pic
    liftIO $ GL.currentColor $= oldColor

preservingMatrix' :: MonadIO m => m () -> m ()
preservingMatrix' m = do
    liftIO $ glPushMatrix
    _ <- m
    liftIO $ glPopMatrix

mapKey :: I.Button -> Either Key MouseButton
mapKey k = case k of
    I.KeyChar c -> Left $ CharKey c
    I.KeySpace -> Left KeySpace
    I.KeyF1 -> Left KeyF1
    I.KeyF2 -> Left KeyF2
    I.KeyF3 -> Left KeyF3
    I.KeyF4 -> Left KeyF4
    I.KeyF5 -> Left KeyF5
    I.KeyF6 -> Left KeyF6
    I.KeyF7 -> Left KeyF7
    I.KeyF8 -> Left KeyF8
    I.KeyF9 -> Left KeyF9
    I.KeyF10 -> Left KeyF10
    I.KeyF11 -> Left KeyF11
    I.KeyF12 -> Left KeyF12
    I.KeyEsc -> Left KeyEsc
    I.KeyUp -> Left KeyUp
    I.KeyDown -> Left KeyDown
    I.KeyLeft -> Left KeyLeft
    I.KeyRight -> Left KeyRight
    I.KeyLeftShift -> Left KeyLeftShift
    I.KeyRightShift -> Left KeyLeftShift
    I.KeyLeftControl -> Left KeyLeftCtrl
    I.KeyRightControl -> Left KeyRightCtrl
    I.KeyTab -> Left KeyTab
    I.KeyEnter -> Left KeyEnter
    I.KeyBackspace -> Left KeyBackspace
    I.KeyInsert -> Left KeyInsert
    I.KeyDelete -> Left KeyDel
    I.KeyPageUp -> Left KeyPageup
    I.KeyPageDown -> Left KeyPagedown
    I.KeyHome -> Left KeyHome
    I.KeyEnd -> Left KeyEnd
    I.KeyPad0 -> Left KeyPad0
    I.KeyPad1 -> Left KeyPad1
    I.KeyPad2 -> Left KeyPad2
    I.KeyPad3 -> Left KeyPad3
    I.KeyPad4 -> Left KeyPad4
    I.KeyPad5 -> Left KeyPad5
    I.KeyPad6 -> Left KeyPad6
    I.KeyPad7 -> Left KeyPad7
    I.KeyPad8 -> Left KeyPad8
    I.KeyPad9 -> Left KeyPad9
    I.KeyPadDivide -> Left KeyPadDivide
    I.KeyPadMultiply -> Left KeyPadMultiply
    I.KeyPadSubtract -> Left KeyPadSubtract
    I.KeyPadAdd -> Left KeyPadAdd
    I.KeyPadDecimal -> Left KeyPadDecimal
    I.KeyPadEqual -> Left KeyPadEqual
    I.KeyPadEnter -> Left KeyPadEnter
    I.MouseLeft -> Right MouseButton0
    I.MouseRight -> Right MouseButton1
    I.MouseMiddle -> Right MouseButton2

gf :: Float -> GL.GLfloat
{-# INLINE gf #-}
gf x = unsafeCoerce x

gsizei :: Int -> GL.GLsizei
{-# INLINE gsizei #-}
gsizei x = unsafeCoerce x
