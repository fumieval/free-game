{-# LANGUAGE ImplicitParams #-}
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
import Linear

runGame :: GUIParam -> F GUI a -> IO (Maybe a)
runGame param m = launch param $ \r s -> runF m (return . Just) (runAction param r s)

instance Given (IORef Int) => MonadTick IO where
    tick = do
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

type Texture = (GL.TextureObject, Int, Int)

bool :: a -> a -> Bool -> a
bool r _ False = r
bool _ r True = r

launch :: GUIParam -> FinalizerT IO (Maybe a) -> IO (Maybe a)
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
    return (tex, width, height)

fromKeyState :: KeyState -> Bool
fromKeyState = undefined

instance Keyboard IO where
    charKey = GLFW.getKey (mapCharKey ch)
    specialKey k = GLFW.getKey (mapSpecialKey x)

instance Given (IORef (IM.IntMap Texture)) => Picture2D (FinalizerT IO) where
    fromBitmap bmp@(BitmapData _ (Just h)) = do
        m <- liftIO $ readIORef given
        case IM.lookup h m of
            Just t -> liftIO $ drawTexture t
            Nothing -> do
                t <- installTexture bmp
                liftIO $ writeIORef ?refTextures $ IM.insert h t m
                liftIO $ drawTexture t
                finalizer $ modifyIORef ?refTextures $ IM.delete h
    fromBitmap bmp@(BitmapData _ Nothing) = liftIO $ runFinalizerT
        $ installTexture bmp >>= liftIO . drawTexture
    rotateD theta m = preservingMatrix' $ do
        liftIO $ GL.rotate (gf (-theta)) (GL.Vector3 0 0 1)
        m
    scale (V2 sx sy) m = preservingMatrix' $ do
        liftIO $ GL.scale (gf sx) (gf sy) 1
        m
    colored col m = do
        oldColor <- liftIO $ get GL.currentColor
        liftIO $ GL.currentColor $= unsafeCoerce col
        r <- m
        liftIO $ GL.currentColor $= oldColor
        return r
    translate (V2 tx ty) m = preservingMatrix' $ do
        liftIO $ GL.translate (GL.Vector3 (gf tx) (gf ty) 0)
        m

instance Figure2D IO where
    circle r = do
        let s = 2 * pi / 64
        GL.renderPrimitive GL.Polygon $ runVertices [V2 (cos t * r) (sin t * r) | t <- [0,s..2 * pi]]
    circleOutline r = do
        let s = 2 * pi / 64 * sc
        GL.renderPrimitive GL.LineLoop $ runVertices [V2 (cos t * r) (sin t * r) | t <- [0,s..2 * pi]]
    polygon path = GL.renderPrimitive GL.Polygon $ runVertices path
    polygonOutline path = GL.renderPrimitive GL.LineLoop $ runVertices path
    line path = GL.renderPrimitive GL.LineStrip $ runVertices path
    thickness t m = do
        oldWidth <- get GL.lineWidth
        GL.lineWidth $= gf t
        r <- m
        GL.lineWidth $= oldWidth
        return r

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

mkVertex2 :: V2 Float -> GL.Vertex2 GL.GLfloat
mkVertex2 = unsafeCoerce

gf :: Float -> GL.GLfloat
{-# INLINE gf #-}
gf x = unsafeCoerce x

gsizei :: Int -> GL.GLsizei
{-# INLINE gsizei #-}
gsizei x = unsafeCoerce x

mapCharKey :: Char -> GLFW.Key
mapCharKey = undefined

mapSpecialKey :: SpecialKey -> GLFW.Key
mapSpecialKey KeySpace = GLFW.Key'Space
mapSpecialKey KeyEsc = GLFW.Key'Esc
mapSpecialKey KeyLeftShift = GLFW.Key'LeftShift
mapSpecialKey KeyRightShift = GLFW.Key'RightShift
mapSpecialKey KeyLeftControl = GLFW.Key'LeftControl
mapSpecialKey KeyRightControl = GLFW.Key'RightControl
mapSpecialKey KeyUp = GLFW.Key'Up
mapSpecialKey KeyDown = GLFW.Key'Down
mapSpecialKey KeyLeft = GLFW.Key'Left
mapSpecialKey KeyRight = GLFW.Key'Right
mapSpecialKey KeyTab = GLFW.Key'Tab
mapSpecialKey KeyEnter = GLFW.Key'Enter
mapSpecialKey KeyBackspace = GLFW.Key'Backspace
mapSpecialKey KeyInsert = GLFW.Key'Insert
mapSpecialKey KeyDelete = GLFW.Key'Del
mapSpecialKey KeyPageUp = GLFW.Key'Pageup
mapSpecialKey KeyPageDown = GLFW.Key'Pagedown
mapSpecialKey KeyHome = GLFW.Key'Home
mapSpecialKey KeyEnd = GLFW.Key'End
mapSpecialKey KeyF1 = GLFW.Key'F1
mapSpecialKey KeyF2 = GLFW.Key'F2
mapSpecialKey KeyF3 = GLFW.Key'F3
mapSpecialKey KeyF4 = GLFW.Key'F4
mapSpecialKey KeyF5 = GLFW.Key'F5
mapSpecialKey KeyF6 = GLFW.Key'F6
mapSpecialKey KeyF7 = GLFW.Key'F7
mapSpecialKey KeyF8 = GLFW.Key'F8
mapSpecialKey KeyF9 = GLFW.Key'F9
mapSpecialKey KeyF10 = GLFW.Key'F10
mapSpecialKey KeyF11 = GLFW.Key'F11
mapSpecialKey KeyF12 = GLFW.Key'F12
mapSpecialKey KeyPad0 = GLFW.Key'Pad0
mapSpecialKey KeyPad1 = GLFW.Key'Pad1
mapSpecialKey KeyPad2 = GLFW.Key'Pad2
mapSpecialKey KeyPad3 = GLFW.Key'Pad3
mapSpecialKey KeyPad4 = GLFW.Key'Pad4
mapSpecialKey KeyPad5 = GLFW.Key'Pad5
mapSpecialKey KeyPad6 = GLFW.Key'Pad6
mapSpecialKey KeyPad7 = GLFW.Key'Pad7
mapSpecialKey KeyPad8 = GLFW.Key'Pad8
mapSpecialKey KeyPad9 = GLFW.Key'Pad9
mapSpecialKey KeyPadDivide = GLFW.Key'PadDivide
mapSpecialKey KeyPadMultiply = GLFW.Key'PadMultiply
mapSpecialKey KeyPadSubtract = GLFW.Key'PadSubtract
mapSpecialKey KeyPadAdd = GLFW.Key'PadAdd
mapSpecialKey KeyPadDecimal = GLFW.Key'PadDecimal
mapSpecialKey KeyPadEqual = GLFW.Key'PadEqual
mapSpecialKey KeyPadEnter = GLFW.Key'PadEnter
