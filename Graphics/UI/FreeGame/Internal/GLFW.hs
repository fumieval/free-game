module Graphics.UI.FreeGame.Internal.GLFW where

import qualified Graphics.UI.GLFW as GLFW
import Unsafe.Coerce

import qualified Data.Array.Repa.Repr.ForeignPtr as RF
import Foreign.ForeignPtr
import qualified Graphics.Rendering.OpenGL.GL as GL
import Graphics.Rendering.OpenGL.GL.StateVar
import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility
import Data.Color
import Graphics.UI.FreeGame.Internal.Finalizer
import Graphics.UI.FreeGame.Data.Bitmap
import Graphics.UI.FreeGame.Types
import Linear
import Data.IORef
import System.Mem
import Control.Bool
import Control.Monad.IO.Class
import Control.Concurrent

data System = System
    { refFrameCounter :: IORef Int
    , refFPS :: IORef Int
    , theFPS :: Int
    , theRegion :: BoundingBox Float
    , theWindow :: GLFW.Window
    }

fromKeyState :: GLFW.KeyState -> Bool
fromKeyState GLFW.KeyState'Pressed = True
fromKeyState GLFW.KeyState'Released = False
fromKeyState GLFW.KeyState'Repeating = True

runVertices :: MonadIO m => [V2 Double] -> m ()
runVertices = liftIO . mapM_ (GL.vertex . mkVertex2)
{-# INLINE runVertices #-}

preservingMatrix' :: MonadIO m => m a -> m a
preservingMatrix' m = do
    liftIO glPushMatrix
    r <- m
    liftIO glPopMatrix
    return r

drawTexture :: (GL.TextureObject, Double, Double) -> IO ()
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

translate :: V2 Double -> IO a -> IO a
translate (V2 tx ty) m = preservingMatrix' $ liftIO (GL.translate (GL.Vector3 (gd tx) (gd ty) 0)) >> m

rotateD :: Double -> IO a -> IO a
rotateD theta m = preservingMatrix' $ liftIO (GL.rotate (gd (-theta)) (GL.Vector3 0 0 1)) >> m

scale :: V2 Double -> IO a -> IO a
scale (V2 sx sy) m = preservingMatrix' $ liftIO (GL.scale (gd sx) (gd sy) 1) >> m

circle :: Double -> IO ()
circle r = do
    let s = 2 * pi / 64
    GL.renderPrimitive GL.Polygon $ runVertices [V2 (cos t * r) (sin t * r) | t <- [0,s..2 * pi]]

circleOutline :: Double -> IO ()
circleOutline r = do
    let s = 2 * pi / 64
    GL.renderPrimitive GL.LineLoop $ runVertices [V2 (cos t * r) (sin t * r) | t <- [0,s..2 * pi]]

colored :: Color -> IO a -> IO a
colored col m = do
    oldColor <- liftIO $ get GL.currentColor
    liftIO $ GL.currentColor $= unsafeCoerce col
    res <- m
    liftIO $ GL.currentColor $= oldColor
    return res

polygon :: [V2 Double] -> IO ()
polygon path = GL.renderPrimitive GL.Polygon $ runVertices path

polygonOutline :: [V2 Double] -> IO ()
polygonOutline path = GL.renderPrimitive GL.LineLoop $ runVertices path

line :: [V2 Double] -> IO ()
line path = GL.renderPrimitive GL.LineStrip $ runVertices path

thickness :: Float -> IO a -> IO a
thickness t m = do
    oldWidth <- liftIO $ get GL.lineWidth
    liftIO $ GL.lineWidth $= gf t
    res <- m
    liftIO $ GL.lineWidth $= oldWidth
    return res

installTexture :: Bitmap -> FinalizerT IO (GL.TextureObject, Double, Double)
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

beginFrame :: System -> IO ()
beginFrame sys = do
    GL.clear [GL.ColorBuffer] 
    GL.loadIdentity
    let BoundingBox wl wt wr wb = fmap realToFrac $ theRegion sys
    GL.ortho wl wr wb wt 0 (-100)
    GL.matrixMode $= GL.Modelview 0

endFrame :: System -> IO ()
endFrame sys = do
    GL.matrixMode $= GL.Projection
    GLFW.swapBuffers $ theWindow sys
    GL.flush
    GLFW.pollEvents
    performGC
    Just t <- GLFW.getTime
    n <- readIORef (refFrameCounter sys)
    threadDelay $ floor $ (1000000 *) $ fromIntegral n / fromIntegral (theFPS sys) - t
    if t > 1
        then GLFW.setTime 0 >> writeIORef (refFrameCounter sys) 0
        else writeIORef (refFrameCounter sys) (succ n)

withGLFW :: String -> Bool -> Int -> Int -> Color -> IO a -> IO a
withGLFW title windowed ww wh color m = do
    () <- unlessM GLFW.init (fail "Failed to initialize")

    mon <- if windowed then GLFW.getPrimaryMonitor else return Nothing

    Just win <- GLFW.createWindow ww wh title mon Nothing

    GL.lineSmooth $= GL.Enabled
    GL.blend      $= GL.Enabled
    GL.blendFunc  $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    GL.shadeModel $= GL.Flat
    GL.textureFunction $= GL.Combine

    GL.clearColor $= unsafeCoerce color

    res <- m

    GLFW.destroyWindow win
    GLFW.terminate
    return res
