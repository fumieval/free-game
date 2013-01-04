{-# LANGUAGE ImplicitParams, ScopedTypeVariables #-}
module Graphics.FreeGame.Backends.GLFW (runGame) where
import Graphics.UI.GLFW as GLFW
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
import Data.StateVar
import qualified Data.Array.Repa.Repr.ForeignPtr as RF
import Foreign.ForeignPtr
import qualified Data.IntMap as IM
import Unsafe.Coerce
import Data.Vect
import System.Mem

data Texture = Texture {texObj :: GL.TextureObject, texWidth :: Int, texHeight :: Int}

installTexture :: Bitmap -> IO Texture
installTexture bmp = do
    [tex] <- GL.genObjectNames 1
    GL.textureBinding GL.Texture2D GL.$= Just tex

    fptr <- liftM RF.toForeignPtr $ computeP $ bitmapData bmp
    let (width, height) = bitmapSize bmp
    withForeignPtr fptr
        $ GL.texImage2D Nothing GL.NoProxy 0 GL.RGBA8 (GL.TextureSize2D (gsizei width) (gsizei height)) 0
        . GL.PixelData GL.RGBA GL.UnsignedInt8888

    return $ Texture tex width height

drawPic :: (?refTextures :: IORef (IM.IntMap Texture)) => Picture -> IO ()
drawPic (Image u) = do
    Texture tex width height <- liftM (IM.! hashUnique u) $ readIORef ?refTextures
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

drawPic (Rotate theta p) = GL.preservingMatrix $ GL.rotate (gf (-theta)) (GL.Vector3 0 0 1) >> drawPic p
drawPic (Scale (Vec2 sx sy) p) = GL.preservingMatrix $ GL.scale (gf sx) (gf sy) 1 >> drawPic p
drawPic (Translate (Vec2 tx ty) p) = GL.preservingMatrix $ GL.translate (GL.Vector3 (gf tx) (gf ty) 0) >> drawPic p
drawPic (Pictures ps) = mapM_ drawPic ps

-- | Run 'Game' using OpenGL and GLFW.
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
    r <- run [] game

    closeWindow
    terminate
    return r
    where
    run :: (?windowWidth :: Int, ?windowHeight :: Int
        , ?refTextures :: IORef (IM.IntMap Texture)
        , ?refFrame :: IORef Int
        , ?frameTime :: Double) => [Int] -> Game a -> IO (Maybe a)
    run is (Pure x) = do
        m <- readIORef ?refTextures
        GL.deleteObjectNames [texObj $ m IM.! i | i <- is]
        modifyIORef ?refTextures $ flip (foldr IM.delete) is
        return (Just x)
    run is (Free f) = case f of
        EmbedIO m -> m >>= run is
        Bracket m -> run [] m >>= maybe (return Nothing) (run is)
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
                then GL.clear [GL.ColorBuffer] >> performGC >> run is cont
                else return Nothing
        AskInput key fcont -> keyIsPressed (mapKey key) >>= run is . fcont
        GetMouseState fcont -> do
            (x, y) <- getMousePosition
            b0 <- mouseButtonIsPressed MouseButton0
            b1 <- mouseButtonIsPressed MouseButton1
            b2 <- mouseButtonIsPressed MouseButton1
            w <- getMouseWheel
            run is $ fcont $ I.MouseState (Vec2 (fromIntegral x) (fromIntegral y)) b0 b2 b1 w
        DrawPicture pic cont -> do
            GL.preservingMatrix $ do
                GL.loadIdentity
                GL.scale (gf 1) (-1) 1
                GL.ortho 0 (fromIntegral ?windowWidth) 0 (fromIntegral ?windowHeight) 0 (-100)
                GL.matrixMode   $= GL.Modelview 0
                drawPic pic
                GL.matrixMode   $= GL.Projection
            run is cont
        LoadPicture bmp fcont -> do
            tex <- installTexture bmp
            u <- newUnique
            modifyIORef ?refTextures $ IM.insert (hashUnique u) tex
            run (hashUnique u:is) $ fcont (Image u)

    mapKey k = case k of
        I.KeyChar c -> CharKey c
        I.KeySpace -> KeySpace
        I.KeyF1 -> KeyF1
        I.KeyF2 -> KeyF2
        I.KeyF3 -> KeyF3
        I.KeyF4 -> KeyF4
        I.KeyF5 -> KeyF5
        I.KeyF6 -> KeyF6
        I.KeyF7 -> KeyF7
        I.KeyF8 -> KeyF8
        I.KeyF9 -> KeyF9
        I.KeyF10 -> KeyF10
        I.KeyF11 -> KeyF11
        I.KeyF12 -> KeyF12
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
        I.KeyInsert -> KeyInsert
        I.KeyDelete -> KeyDel
        I.KeyPageUp -> KeyPageup
        I.KeyPageDown -> KeyPagedown
        I.KeyHome -> KeyHome
        I.KeyEnd -> KeyEnd
        I.KeyPad0 -> KeyPad0
        I.KeyPad1 -> KeyPad1
        I.KeyPad2 -> KeyPad2
        I.KeyPad3 -> KeyPad3
        I.KeyPad4 -> KeyPad4
        I.KeyPad5 -> KeyPad5
        I.KeyPad6 -> KeyPad6
        I.KeyPad7 -> KeyPad7
        I.KeyPad8 -> KeyPad8
        I.KeyPad9 -> KeyPad9
        I.KeyPadDivide -> KeyPadDivide
        I.KeyPadMultiply -> KeyPadMultiply
        I.KeyPadSubtract -> KeyPadSubtract
        I.KeyPadAdd -> KeyPadAdd
        I.KeyPadDecimal -> KeyPadDecimal
        I.KeyPadEqual -> KeyPadEqual
        I.KeyPadEnter -> KeyPadEnter

gf :: Float -> GL.GLfloat
{-# INLINE gf #-}
gf x = unsafeCoerce x

gsizei :: Int -> GL.GLsizei
{-# INLINE gsizei #-}
gsizei x = unsafeCoerce x
