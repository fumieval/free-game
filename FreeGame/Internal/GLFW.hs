{-# LANGUAGE BangPatterns #-}
module FreeGame.Internal.GLFW where
import Control.Concurrent
import Control.Bool
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Control.Monad.IO.Class
import Control.Lens
import Data.Color
import Data.IORef
import Data.Word
import FreeGame.Types
import Data.BoundingBox
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Storable
import Foreign.Ptr
import Linear
import Graphics.GL
import Graphics.GL.Compatibility32
import qualified Graphics.Holz as Holz
import qualified Graphics.UI.GLFW as GLFW
import Unsafe.Coerce
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Codec.Picture
import qualified GHC.IO.Encoding as Encoding
import qualified Data.Sequence as S
import Data.Foldable

data System = System
    { refFrameCounter :: IORef Int
    , theFPS :: IORef Double
    , frameTimes :: IORef (S.Seq Double)
    , currentFPS :: IORef Int
    , startTime :: IORef Double
    , refRegion :: IORef BoundingBox2
    , theWindow :: GLFW.Window
    }

trim1 :: S.Seq Double -> S.Seq Double
trim1 s0 = go zs s0 (sum zs) where
    go (x:xs) s a
        | a < 1 = s
        | otherwise = go xs (S.drop 1 s) (a - x)
    go [] s _ = s
    zs = toList s0

type Texture = (Holz.Texture, Double, Double)

runVertices :: MonadIO m => [V2 Double] -> m ()
runVertices = liftIO . mapM_ (\(V2 x y) -> glVertex2d x y)
{-# INLINE runVertices #-}

preservingMatrix' :: MonadIO m => m a -> m a
preservingMatrix' m = do
    liftIO glPushMatrix
    r <- m
    liftIO glPopMatrix
    return r
{-# INLINE preservingMatrix' #-}

drawTexture :: Texture -> IO ()
drawTexture (tex, !w, !h) = drawTextureAt tex (V2 (-w) (-h)) (V2 w (-h)) (V2 w h) (V2 (-w) h)
{-# INLINE drawTexture #-}

drawTextureAt :: Holz.Texture -> V2 Double -> V2 Double -> V2 Double -> V2 Double -> IO ()
drawTextureAt tex (V2 ax ay) (V2 bx by) (V2 cx cy) (V2 dx dy) = do
    glEnable GL_TEXTURE_2D
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR_MIPMAP_LINEAR
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR
    glBindTexture GL_TEXTURE_2D (unsafeCoerce tex)

    glBegin GL_TRIANGLE_STRIP

    glTexCoord2d 0 0
    glVertex2d ax ay
    glTexCoord2d 1 0
    glVertex2d bx by
    glTexCoord2d 0 1
    glVertex2d dx dy
    glTexCoord2d 1 1
    glVertex2d cx cy

    glEnd

    glDisable GL_TEXTURE_2D

gf :: Float -> GLfloat
{-# INLINE gf #-}
gf = unsafeCoerce

gd :: Double -> GLdouble
{-# INLINE gd #-}
gd = unsafeCoerce

gsizei :: Int -> GLsizei
{-# INLINE gsizei #-}
gsizei = unsafeCoerce

translate :: V2 Double -> IO a -> IO a
translate (V2 tx ty) m = preservingMatrix' $ glTranslated (gd tx) (gd ty) 0 >> m

rotateD :: Double -> IO a -> IO a
rotateD theta m = preservingMatrix' $ glRotated (gd (-theta)) 0 0 1 >> m

scale :: V2 Double -> IO a -> IO a
scale (V2 sx sy) m = preservingMatrix' $ glScaled (gd sx) (gd sy) 1 >> m

renderPrimitive :: GLenum -> IO a -> IO a
renderPrimitive mode m = do
    glBegin mode
    r <- m
    glEnd
    return r
{-# INLINE renderPrimitive #-}

circle :: Double -> IO ()
circle r = do
    let s = 2 * pi / 64
    renderPrimitive GL_POLYGON $ runVertices [V2 (cos t * r) (sin t * r) | t <- [0,s..2 * pi]]

circleOutline :: Double -> IO ()
circleOutline r = do
    let s = 2 * pi / 64
    renderPrimitive GL_LINE_LOOP $ runVertices [V2 (cos t * r) (sin t * r) | t <- [0,s..2 * pi]]

color :: Color Float -> IO a -> IO a
color (V4 r g b a) m = do
    V4 r0 g0 b0 a0 <- overPtr $ glGetFloatv GL_CURRENT_COLOR . castPtr
    glColor4f r g b a
    res <- m
    glColor4f r0 g0 b0 a0
    return res

polygon :: [V2 Double] -> IO ()
polygon = renderPrimitive GL_POLYGON . runVertices

polygonOutline :: [V2 Double] -> IO ()
polygonOutline = renderPrimitive GL_LINE_LOOP . runVertices

line :: [V2 Double] -> IO ()
line = renderPrimitive GL_LINE_STRIP . runVertices

thickness :: Float -> IO a -> IO a
thickness t m = do
    oldWidth <- overPtr $ glGetFloatv GL_LINE_WIDTH
    glLineWidth (gf t)
    res <- m
    glLineWidth oldWidth
    return res

installTexture :: Image PixelRGBA8 -> IO Texture
installTexture img@(Image w h _) = do
    tex <- Holz.registerTexture img
    return (tex, fromIntegral w / 2, fromIntegral h / 2)

releaseTexture :: Texture -> IO ()
releaseTexture (tex, _, _) = Holz.releaseTexture tex

beginFrame :: System -> IO ()
beginFrame sys = do
    Box (V2 wl wt) (V2 wr wb) <- fmap realToFrac <$> readIORef (refRegion sys)
    glViewport 0 0 (floor $ wr - wl) (floor $ wb - wt)
    glMatrixMode GL_PROJECTION
    glLoadIdentity
    glOrtho wl wr wb wt 0 (-100)
    glMatrixMode GL_MODELVIEW
    glClear GL_COLOR_BUFFER_BIT
    Just t <- GLFW.getTime
    writeIORef (startTime sys) t

endFrame :: System -> IO Bool
endFrame sys = do
    GLFW.swapBuffers $ theWindow sys
    GLFW.pollEvents
    Just t <- GLFW.getTime
    n <- readIORef (refFrameCounter sys)
    fps <- readIORef (theFPS sys)
    threadDelay $ max 0 $ floor $ (1000000 *) $ fromIntegral n / fps - t
    writeIORef (refFrameCounter sys) (succ n)
    t0 <- readIORef (startTime sys)
    fs <- readIORef (frameTimes sys)
    Just t1 <- GLFW.getTime
    let !fs' = trim1 $ fs S.|> (t1 - t0)
    writeIORef (currentFPS sys) (S.length fs')
    writeIORef (frameTimes sys) fs'
    GLFW.windowShouldClose (theWindow sys)

withGLFW :: WindowMode -> BoundingBox2 -> (System -> IO a) -> IO a
withGLFW mode bbox@(Box (V2 x0 y0) (V2 x1 y1)) m = do
    Encoding.setForeignEncoding Encoding.utf8
    let title = "free-game"
        ww = floor $ x1 - x0
        wh = floor $ y1 - y0
    () <- unlessM GLFW.init (fail "Failed to initialize")

    mon <- case mode of
        FullScreen -> GLFW.getPrimaryMonitor
        _ -> return Nothing

    GLFW.windowHint $ GLFW.WindowHint'Resizable $ mode == Resizable
    win <- GLFW.createWindow ww wh title mon Nothing >>= maybe (fail "Failed to create a window") return
    GLFW.makeContextCurrent (Just win)
    glEnable GL_LINE_SMOOTH
    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
    -- GL.shadeModel $= GL.Flat
    glTexEnvi GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_COMBINE

    GLFW.swapInterval 1
    glClearColor 1 1 1 1

    rbox <- newIORef bbox

    GLFW.setFramebufferSizeCallback win $ Just $ \_ w h -> do
        modifyIORef rbox $ size zero .~ fmap fromIntegral (V2 w h)

    sys <- System
        <$> newIORef 0
        <*> newIORef 60
        <*> newIORef S.empty
        <*> newIORef 60
        <*> newIORef 0
        <*> pure rbox
        <*> pure win

    res <- m sys

    GLFW.destroyWindow win
    GLFW.terminate
    return res

-- FIXME: copypasted from holz
screenshot :: System -> IO (Image PixelRGBA8)
screenshot sys = do
    V2 w h <- fmap floor <$> view (size zero) <$> readIORef (refRegion sys)
    mv <- MV.unsafeNew $ w * h * 4 :: IO (MV.IOVector Word8)
    mv' <- MV.unsafeNew $ w * h * 4
    glReadBuffer GL_FRONT
    MV.unsafeWith mv $ \ptr -> do
      glReadPixels 0 0 (fromIntegral w) (fromIntegral h) GL_RGBA GL_UNSIGNED_BYTE (castPtr ptr)
      MV.unsafeWith mv' $ \dst -> forM_ [0..h-1] $ \y -> copyBytes (plusPtr dst $ y * w * 4) (plusPtr ptr $ (h - y - 1) * w * 4) (4 * w)
    Image w h <$> V.unsafeFreeze mv'

blendMode2BlendingFactors :: BlendMode -> (GLenum, GLenum)
blendMode2BlendingFactors Normal = (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
blendMode2BlendingFactors Inverse = (GL_ONE_MINUS_DST_COLOR, GL_ZERO)
blendMode2BlendingFactors Add = (GL_SRC_ALPHA, GL_ONE)
blendMode2BlendingFactors Multiply = (GL_ZERO, GL_SRC_COLOR)
blendMode2BlendingFactors Screen = (GL_ONE, GL_ONE_MINUS_SRC_COLOR)

blendMode :: BlendMode -> IO a -> IO a
blendMode mode m = do
    fs <- overPtr $ glGetIntegerv GL_BLEND_SRC_RGB
    fd <- overPtr $ glGetIntegerv GL_BLEND_DST_RGB
    uncurry glBlendFunc $ blendMode2BlendingFactors mode
    a <- m
    glBlendFunc (fromIntegral fs) (fromIntegral fd)
    return a

overPtr :: (Storable a) => (Ptr a -> IO b) -> IO a
overPtr f = alloca $ \p -> f p >> peek p
{-# INLINE overPtr #-}
