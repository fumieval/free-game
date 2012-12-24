{-# LANGUAGE ImplicitParams, ScopedTypeVariables #-}
module Graphics.FreeGame.Backends.GLFW (runGame) where
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.GL (($=))
import qualified Graphics.Rendering.OpenGL.GL as GL
import Graphics.FreeGame.Base
import Graphics.FreeGame.Bitmap
import qualified Graphics.FreeGame.Input as I
import Codec.Picture.Repa
import Control.Monad.Free
import Control.Monad
import System.Random
import Data.Unique
import Data.IORef
import Data.Array.Repa as R
import qualified Data.Array.Repa.Repr.ForeignPtr as RF
import Foreign.ForeignPtr
import qualified Data.IntMap as IM
import GHC.Float
import Unsafe.Coerce

type Texture = (GL.TextureObject, Int, Int)

installTexture :: Bitmap -> IO Texture
installTexture bmp = do
    [tex] <- GL.genObjectNames 1
    GL.textureBinding GL.Texture2D GL.$= Just tex

    fptr <- liftM RF.toForeignPtr $ R.computeP $ bitmapData bmp
    let (width, height) = bitmapSize bmp
    withForeignPtr fptr
        $ GL.texImage2D Nothing GL.NoProxy 0 GL.RGBA8 (GL.TextureSize2D (gsizei width) (gsizei height)) 0
        . GL.PixelData GL.RGBA GL.UnsignedInt8888

    return (tex, width, height)

freeTexture :: Texture -> IO ()
freeTexture (tex, _, _) = GL.deleteObjectNames [tex]

drawPic :: (?refTextures :: IORef (IM.IntMap Texture)) => Picture -> IO ()
drawPic (Image u) = do
    (tex, width, height) <- liftM (IM.! hashUnique u) $ readIORef ?refTextures
    let (w, h) = (fromIntegral width / 2, fromIntegral height / 2)
    GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.Repeat)
    GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.Repeat)
    GL.textureFilter   GL.Texture2D      $= ((GL.Nearest, Nothing), GL.Nearest)
    GL.texture GL.Texture2D $= GL.Enabled
    GL.textureFunction      $= GL.Combine    
    GL.textureBinding GL.Texture2D $= Just tex
    GL.currentColor $= GL.Color4 1.0 1.0 1.0 1.0
    GL.renderPrimitive GL.Polygon $ zipWithM_
        (\(pX, pY) (tX, tY) -> do
            GL.texCoord $ GL.TexCoord2 (gf tX) (gf tY)
            GL.vertex $ GL.Vertex2   (gf pX) (gf pY))
        [(-w, -h), (w, -h), (w, h), (-w, h)]
        [(0,0), (1.0,0), (1.0,1.0), (0,1.0)]
    GL.texture GL.Texture2D GL.$= GL.Disabled

drawPic (Rotate theta p) = GL.preservingMatrix $ GL.rotate (gf theta) (GL.Vector3 0 0 (-1)) >> drawPic p
drawPic (Scale (sx, sy) p) = GL.preservingMatrix $ GL.scale (gf sx) (gf sy) 1 >> drawPic p
drawPic (Translate (tx, ty) p) = GL.preservingMatrix $ GL.translate (GL.Vector3 (gf tx) (gf ty) 0) >> drawPic p
drawPic (Pictures ps) = mapM_ drawPic ps

runGame :: GameParam -> Game a -> IO (Maybe a)
runGame param game = do
    initialize
    pf <- openGLProfile
    let ?windowWidth = fst $ windowSize param
        ?windowHeight = snd $ windowSize param
    openWindow $ defaultDisplayOptions {
        displayOptions_width = fromIntegral ?windowWidth
        ,displayOptions_height = fromIntegral ?windowHeight
        ,displayOptions_displayMode = Window
        ,displayOptions_windowIsResizable = False
        ,displayOptions_openGLProfile = pf
    }
    setWindowTitle $ windowTitle param
    GL.lineSmooth $= GL.Enabled
    GL.blend      $= GL.Enabled
    GL.blendFunc  $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    GL.shadeModel $= GL.Smooth
    GL.clearColor $= GL.Color4 1 1 1 1
    ref <- newIORef IM.empty
    ref' <- newIORef 0
    let ?refTextures = ref
        ?refFrame = ref'
        ?frameTime = 1 / fromIntegral (framePerSecond param)
    r <- run game

    closeWindow
    terminate
    return r
    where
    run (Pure x) = return (Just x)
    run (Free f) = case f of
        Tick cont -> do
            swapBuffers

            t <- getTime
            n <- readIORef ?refFrame
            sleep (fromIntegral n * ?frameTime - t)
            if t > 1
                then resetTime >> writeIORef ?refFrame 0
                else writeIORef ?refFrame (succ n)

            r <- windowIsOpen
            if r
                then GL.clear [GL.ColorBuffer] >> run cont
                else return Nothing
        AskInput key fcont -> keyIsPressed (mapKey key) >>= run . fcont
        EmbedIO m -> m >>= run
        DrawPicture pic cont -> do
            GL.preservingMatrix $ do
                GL.loadIdentity
                GL.scale (gf 1) (-1) 1
                GL.ortho 0 (fromIntegral ?windowWidth) 0 (fromIntegral ?windowHeight) 0 (-100)
                GL.matrixMode   $= GL.Modelview 0
                drawPic pic
                GL.matrixMode   $= GL.Projection
            run cont
        LoadPicture bmp fcont -> do
            tex <- installTexture bmp
            u <- newUnique
            modifyIORef ?refTextures $ IM.insert (hashUnique u) tex
            run $ fcont (Image u)
    mapKey k = case k of
        I.KeyChar c -> CharKey c
        I.KeySpace -> KeySpace
        I.KeyEsc -> KeyEsc
        I.KeyUp -> KeyUp
        I.KeyDown -> KeyDown
        I.KeyLeft -> KeyLeft
        I.KeyRight -> KeyRight
        I.KeyLeftShift -> KeyLeftShift
        I.KeyRightShift -> KeyLeftShift
        I.KeyLeftControl -> KeyLeftCtrl
        I.KeyRightControl -> KeyRightCtrl
        I.KeyTab -> KeyTab
        I.KeyEnter -> KeyEnter
        I.KeyBackspace -> KeyBackspace

gf :: Double -> GL.GLfloat
{-# INLINE gf #-}
gf x = unsafeCoerce $ double2Float x

gsizei :: Int -> GL.GLsizei
{-# INLINE gsizei #-}
gsizei x = unsafeCoerce x
