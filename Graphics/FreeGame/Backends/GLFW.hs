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
{-# LANGUAGE ImplicitParams, ScopedTypeVariables #-}
module Graphics.FreeGame.Backends.GLFW (runGame) where
import Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL.GL as GL
import Graphics.FreeGame.Base
import Graphics.FreeGame.Data.Bitmap
import Graphics.FreeGame.Data.Color
import qualified Graphics.FreeGame.Input as I
import Control.Applicative
import Control.Monad.Free
import Control.Monad
import Data.IORef
import Data.StateVar
import qualified Data.Array.Repa.Repr.ForeignPtr as RF
import Foreign.ForeignPtr
import qualified Data.IntMap as IM
import Unsafe.Coerce
import System.Mem

data Texture = Texture GL.TextureObject Int Int

installTexture :: Bitmap -> IO Texture
installTexture bmp = do
    [tex] <- GL.genObjectNames 1
    GL.textureBinding GL.Texture2D GL.$= Just tex

    let (width, height) = bitmapSize bmp
    withForeignPtr (RF.toForeignPtr $ bitmapData bmp)
        $ GL.texImage2D Nothing GL.NoProxy 0 GL.RGBA8 (GL.TextureSize2D (gsizei width) (gsizei height)) 0
        . GL.PixelData GL.RGBA GL.UnsignedInt8888
    return $ Texture tex width height

freeTexture :: Texture -> IO ()
freeTexture (Texture tex _ _) = GL.deleteObjectNames [tex]

drawTexture :: Texture -> IO ()
drawTexture (Texture tex width height) = do
    let (w, h) = (fromIntegral width / 2, fromIntegral height / 2)
    GL.textureFilter   GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)
    GL.textureBinding GL.Texture2D $= Just tex
    GL.renderPrimitive GL.Polygon $ zipWithM_
        (\(pX, pY) (tX, tY) -> do
            GL.texCoord $ GL.TexCoord2 (gf tX) (gf tY)
            GL.vertex $ GL.Vertex2   (gf pX) (gf pY))
        [(-w, -h), (w, -h), (w, h), (-w, h)]
        [(0,0), (1.0,0), (1.0,1.0), (0,1.0)]

drawPic :: (?refTextures :: IORef (IM.IntMap Texture)) => Picture -> IO [Int]
drawPic (BitmapPicture bmp) = case bitmapHash bmp of
    Nothing -> do
        t <- installTexture bmp
        drawTexture t
        freeTexture t
        return []
    Just h -> do
        m <- readIORef ?refTextures
        case IM.lookup h m of
            Just t -> [] <$ drawTexture t
            Nothing -> do
                t <- installTexture bmp
                writeIORef ?refTextures $ IM.insert h t m
                drawTexture t
                return [h]

drawPic (Rotate theta p) = GL.preservingMatrix $ GL.rotate (gf (-theta)) (GL.Vector3 0 0 1) >> drawPic p
drawPic (Scale (Vec2 sx sy) p) = GL.preservingMatrix $ GL.scale (gf sx) (gf sy) 1 >> drawPic p
drawPic (Translate (Vec2 tx ty) p) = GL.preservingMatrix $ GL.translate (GL.Vector3 (gf tx) (gf ty) 0) >> drawPic p
drawPic (Pictures ps) = concat <$> mapM drawPic ps
drawPic (IOPicture m) = m >>= drawPic
drawPic (Colored (Color r g b a) pic) = do
    oldColor <- get GL.currentColor
    GL.currentColor  $= GL.Color4 (gf r) (gf g) (gf b) (gf a)
    xs <- drawPic pic
    GL.currentColor $= oldColor
    return xs

run :: (?windowWidth :: Int, ?windowHeight :: Int
    , ?refTextures :: IORef (IM.IntMap Texture)
    , ?refFrame :: IORef Int
    , ?frameTime :: Double
    , ?windowTitle :: String
    , ?windowMode :: Bool
    , ?cursorVisible :: Bool
    ) => [Int] -> Game a -> IO (Maybe a)
run is (Free f) = case f of
    DrawPicture pic cont -> do
        ls <- drawPic pic
        flip run cont $! ls Prelude.++ is -- Strict!!!
    EmbedIO m -> m >>= run is
    Bracket m -> run [] m >>= maybe (return Nothing) (run is)
    Tick cont -> do
        GL.matrixMode   $= GL.Projection
        swapBuffers
        t <- getTime
        n <- readIORef ?refFrame
        sleep (fromIntegral n * ?frameTime - t)
        if t > 1
            then resetTime >> writeIORef ?refFrame 0
            else writeIORef ?refFrame (succ n)

        r <- windowIsOpen
        if r
            then do
                GL.clear [GL.ColorBuffer] 
                performGC
                GL.preservingMatrix $ do
                GL.loadIdentity
                GL.scale (gf 1) (-1) 1
                GL.ortho 0 (fromIntegral ?windowWidth) 0 (fromIntegral ?windowHeight) 0 (-100)
                GL.matrixMode   $= GL.Modelview 0
                run is cont
            else return Nothing
    AskInput key fcont -> either keyIsPressed mouseButtonIsPressed (mapKey key) >>= run is . fcont
    GetMousePosition fcont -> do
        (x, y) <- GLFW.getMousePosition
        run is $ fcont $ Vec2 (fromIntegral x) (fromIntegral y)
    GetMouseWheel fcont -> GLFW.getMouseWheel >>= run is . fcont
    GetGameParam fcont -> do
        dim <- GLFW.getWindowDimensions
        GL.Color4 r g b a <- get GL.clearColor
        run is $ fcont $ GameParam { framePerSecond = floor $ 1 / ?frameTime
                                   , windowSize = dim
                                   , windowTitle = ?windowTitle
                                   , windowed = ?windowMode
                                   , cursorVisible = ?cursorVisible
                                   , clearColor = Color (realToFrac r) 
                                                        (realToFrac g) 
                                                        (realToFrac b) 
                                                        (realToFrac a)
                                   }
    QuitGame -> return Nothing
run is (Pure x) = do
    m <- readIORef ?refTextures
    GL.deleteObjectNames [obj | i <- is, let Texture obj _ _ = m IM.! i]
    modifyIORef ?refTextures $ flip (foldr IM.delete) is
    return (Just x)

-- | Run 'Game' using OpenGL and GLFW.
runGame :: GameParam -> Game a -> IO (Maybe a)
runGame param game = do
    True <- initialize
    pf <- openGLProfile
    let ?windowWidth = fst $ windowSize param
        ?windowHeight = snd $ windowSize param
        ?windowTitle = windowTitle param
        ?windowMode = windowed param
        ?cursorVisible = cursorVisible param
    True <- openWindow $ defaultDisplayOptions {
        displayOptions_width = fromIntegral ?windowWidth
        ,displayOptions_height = fromIntegral ?windowHeight
        ,displayOptions_displayMode = if ?windowMode then Window else Fullscreen
        ,displayOptions_windowIsResizable = False
        ,displayOptions_openGLProfile = pf
    }
    
    if ?cursorVisible then enableMouseCursor 
                      else disableMouseCursor

    setWindowTitle $ ?windowTitle
    
    GL.lineSmooth $= GL.Enabled
    GL.blend      $= GL.Enabled
    GL.blendFunc  $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    GL.shadeModel $= GL.Smooth
    GL.texture GL.Texture2D $= GL.Enabled
    GL.textureFunction $= GL.Combine

    let Color r g b a = clearColor param in GL.clearColor $= GL.Color4 (gf r) (gf g) (gf b) (gf a)

    ref <- newIORef IM.empty
    ref' <- newIORef 0
    let ?refTextures = ref
        ?refFrame = ref'
        ?frameTime = 1 / fromIntegral (framePerSecond param)
    r <- run [] game

    closeWindow
    terminate
    return r

mapKey :: I.Key -> Either Key MouseButton
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
