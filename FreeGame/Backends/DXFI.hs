{-# LANGUAGE ForeignFunctionInterface #-}
module FreeGame.Backends.DXFI (runGame) where
import FreeGame.Base
import FreeGame.Graphic
import FreeGame.Input
import FreeGame.Sound
import Control.Concurrent
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Free
import Data.Vect.Double
import Data.Unique
import Data.Word
import qualified Data.IntMap as IM
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import Codec.Picture.Repa
import Data.Array.Repa
import System.FilePath.Windows
import System.Random

type Handle = CInt
foreign import ccall "DXFI_LoadImage" dxfi_LoadImage :: CWString -> IO Handle
foreign import ccall "DXFI_LoadImageMatrix" dxfi_LoadImageMatrix :: CWString -> Int -> Int -> Int -> Int -> Int -> Ptr Handle -> IO ()
foreign import ccall "DXFI_CropImage" dxfi_CropImage :: Int -> Int -> Int -> Int -> Handle -> IO Handle
foreign import ccall unsafe "DXFI_DrawImage" dxfi_DrawImage :: Int -> Int -> Handle -> Bool -> IO ()
foreign import ccall unsafe "DXFI_DrawImageScaledWithAngle" dxfi_DrawImageBy :: Int -> Int -> Double -> Double -> Handle -> Bool -> Bool -> IO ()
foreign import ccall "DXFI_IsKeyPressed" dxfi_IsKeyPressed :: Int -> IO Bool
foreign import ccall "DXFI_Initialize" dxfi_Initialize :: IO ()
foreign import ccall "DXFI_Release" dxfi_Release :: IO ()
foreign import ccall "DXFI_GetTickCount" dxfi_GetTickCount :: Bool -> IO Int
foreign import ccall "DXFI_SetLogging" dxfi_SetLogging :: Bool -> IO ()
foreign import ccall "DXFI_SetWindowMode" dxfi_SetWindowMode :: Bool -> IO ()
foreign import ccall "DXFI_SetWindowCaption" dxfi_SetWindowCaption :: CWString -> IO ()
foreign import ccall "DXFI_SetDrawingDestination" dxfi_SetDrawingDestination :: Int -> IO ()
foreign import ccall "DXFI_Wait" dxfi_Wait :: Int -> IO ()
foreign import ccall "DXFI_ClearScreen" dxfi_ClearScreen :: Ptr () -> IO ()
foreign import ccall "DXFI_SetBackgroundColor" dxfi_setBackgroundColor :: Int -> Int -> Int -> IO ()
foreign import ccall "DXFI_FlipScreen" dxfi_FlipScreen :: IO ()
foreign import ccall "DXFI_AcceptMessage" processMessage  :: IO Int
foreign import ccall "DXFI_SetSoundVolumeOnce" dxfi_SetSoundVolumeOnce :: Int -> Handle -> IO ()
foreign import ccall "DXFI_SetSoundPanOnce" dxfi_SetSoundPanOnce :: Int -> Handle -> IO ()
foreign import ccall "DXFI_LoadSound" dxfi_LoadSound :: CWString -> Int -> Int -> IO Handle
foreign import ccall "DXFI_PlaySound" dxfi_PlaySound :: Handle -> Int -> Bool -> IO ()
foreign import ccall "DXFI_IsPlayingSound" dxfi_IsPlayingSound :: Handle -> IO Bool
foreign import ccall "DXFI_CreateARGB8ColorBaseImage" dxfi_CreateARGB8ColorBaseImage :: Int -> Int -> Ptr a -> IO ()
foreign import ccall unsafe "DXFI_SetPixelBaseImage" dxfi_SetPixelBaseImage :: Ptr a -> Int -> Int -> Word8 -> Word8 -> Word8 -> Word8 -> IO ()
foreign import ccall "DXFI_ReleaseBaseImage" dxfi_ReleaseBaseImage :: Ptr a -> IO ()
foreign import ccall "DXFI_CreateGraphFromBaseImage" dxfi_CreateGraphFromBaseImage :: Ptr a -> IO Handle
foreign import ccall "DXFI_Helper_SizeOfBaseImage" dxfi_Helper_SizeOfBaseImage :: IO Int

loadImage :: ImageData -> IM.IntMap Handle -> IO (Picture, IM.IntMap Handle)
loadImage img' m = do
    let img = imgData img'
    u <- newUnique
    let Z :. height :. width :. _ = extent img
    bimg <- dxfi_Helper_SizeOfBaseImage >>= mallocBytes
    dxfi_CreateARGB8ColorBaseImage width height bimg
    forM_ [0..height-1] $ \y ->
        forM_ [0..width-1] $ \x ->
            let r = (img !) $ Z :. y :. x :. 3
                g = (img !) $ Z :. y :. x :. 2
                b = (img !) $ Z :. y :. x :. 1
                a = (img !) $ Z :. y :. x :. 0
                in dxfi_SetPixelBaseImage bimg x y r g b a
    h <- dxfi_CreateGraphFromBaseImage bimg
    dxfi_ReleaseBaseImage bimg
    free bimg
    
    return (Image u, IM.insert (hashUnique u) h m)

drawPicture :: IM.IntMap Handle -> Picture -> IO ()
drawPicture m = draw (zero, 1, 0) where
    draw (Vec2 x y, s, a) (Image u) = dxfi_DrawImageBy (floor x) (floor y) s a (m IM.! hashUnique u) True False
    draw (p, s, a) (Translate q x) = draw (p &+ q, s, a) x
    draw (p, s, a) (Rotate t x) = draw (p, s, a + t) x
    draw t (Pictures xs) = mapM_ (draw t) xs
    draw (p, s, a) (Scale k x) = draw (p &* k, s * k, a) x

loadSound :: FilePath -> IM.IntMap Handle -> IO (WaveData, IM.IntMap Handle)
loadSound path m = do
    u <- newUnique
    h <- withCWString path $ \x -> dxfi_LoadSound x 3 (-1)
    return (WaveData u, IM.insert (hashUnique u) h m)

playSound :: IM.IntMap Handle -> Sound -> IO ()
playSound m (Wave (WaveData u)) = do
    let h = m IM.! hashUnique u
    dxfi_PlaySound h 1 True

data SystemState = SystemState
    {
        sysLoadedImages :: IM.IntMap Handle
        ,sysLoadedSounds :: IM.IntMap Handle
        ,sysStartTime :: Int
        ,sysFrameCount :: Int
    }

runGame :: Bool -- ^window mode
    -> String -- ^window title
    -> Int -- ^frames (per second)
    -> Free Game a -- ^the computation
    -> IO (Maybe a) -- ^result
runGame windowed title fps game = do
    dxfi_SetWindowMode windowed
    dxfi_SetLogging False
    withCWString title dxfi_SetWindowCaption
    dxfi_Initialize
    dxfi_SetDrawingDestination (-2)
    time <- dxfi_GetTickCount False
    result <- run game `evalStateT` SystemState IM.empty IM.empty time 0
    
    dxfi_Release
    
    return result
    where        
        run :: Free Game a -> StateT SystemState IO (Maybe a)
        run (Pure a) = return (Just a)
        run (Free x) = case x of
            DrawPicture pic cont -> do
                st <- get
                lift $ drawPicture (sysLoadedImages st) pic
                run cont
            PlaySound sound cont -> do
                st <- get
                lift $ playSound (sysLoadedSounds st) sound
                run cont
            AskInput key cont -> lift (isPressed key) >>= run . cont
            Randomness r cont -> lift (randomRIO r) >>= run . cont
            LoadImage path cont -> do
                st <- get
                (img, imgs) <- lift $ loadImage path $ sysLoadedImages st
                put $ st { sysLoadedImages = imgs }
                run $ cont img
            LoadSound path cont -> do
                st <- get
                (sound, sounds) <- lift $ loadSound path (sysLoadedImages st)
                put $ st { sysLoadedSounds = sounds }
                run $ cont sound
            EmbedIO m -> lift m >>= run
            Tick cont -> do
                lift dxfi_FlipScreen
                
                time <- lift $ dxfi_GetTickCount False
                st@(SystemState _ _ startTime frameCount) <- get
                lift $ dxfi_Wait $ (frameCount * 1000) `div` fps - time + startTime
                
                if time - startTime >= 1000
                    then put $ st {sysStartTime　= time, sysFrameCount = 0}
                    else put $ st {sysFrameCount = succ frameCount}
                
                p <- lift processMessage

                if (p == 0)
                    then pre >> run cont
                    else return Nothing
                where
                    pre = do
                        lift (dxfi_ClearScreen nullPtr)
                        lift $ dxfi_setBackgroundColor 255 255 255
isPressed = dxfi_IsKeyPressed . k
    where
        k KeyEsc = 0x01
        k (KeyChar '1') = 0x02
        k (KeyChar '2') = 0x03
        k (KeyChar '3') = 0x04
        k (KeyChar '4') = 0x05
        k (KeyChar '5') = 0x06
        k (KeyChar '6') = 0x07
        k (KeyChar '7') = 0x08
        k (KeyChar '8') = 0x09
        k (KeyChar '9') = 0x0A
        k (KeyChar '0') = 0x0B
        k (KeyChar '-') = 0x0C
        k KeyBackspace  = 0x0E
        k KeyTab = 0x0F
        k (KeyChar 'Q') = 0x10
        k (KeyChar 'W') = 0x11
        k (KeyChar 'E') = 0x12
        k (KeyChar 'R') = 0x13
        k (KeyChar 'T') = 0x14
        k (KeyChar 'Y') = 0x15
        k (KeyChar 'U') = 0x16
        k (KeyChar 'I') = 0x17
        k (KeyChar 'O') = 0x18
        k (KeyChar 'P') = 0x19
        k (KeyChar '[') = 0x1A
        k (KeyChar ']') = 0x1B
        k KeyEnter = 0x1C
        k KeyLeftControl = 0x1D
        k (KeyChar 'A') = 0x1E
        k (KeyChar 'S') = 0x1F
        k (KeyChar 'D') = 0x20
        k (KeyChar 'F') = 0x21
        k (KeyChar 'G') = 0x22
        k (KeyChar 'H') = 0x23
        k (KeyChar 'J') = 0x24
        k (KeyChar 'K') = 0x25
        k (KeyChar 'L') = 0x26
        k (KeyChar ';') = 0x27
        k KeyLeftShift = 0x2A
        k (KeyChar '\\') = 0x2B
        k (KeyChar 'Z') = 0x2C
        k (KeyChar 'X') = 0x2D
        k (KeyChar 'C') = 0x2E
        k (KeyChar 'V') = 0x2F
        k (KeyChar 'B') = 0x30
        k (KeyChar 'N') = 0x31
        k (KeyChar 'M') = 0x32
        k (KeyChar ',') = 0x33
        k (KeyChar '.') = 0x34
        k (KeyChar '/') = 0x35
        k KeyRightShift = 0x36
        k KeySpace = 0x39
        k KeyF1 = 0x3B
        k KeyF2 = 0x3C
        k KeyF3 = 0x3D
        k KeyF4 = 0x3E
        k KeyF5 = 0x3F
        k KeyF6 = 0x40
        k KeyF7 = 0x41
        k KeyF8 = 0x42
        k KeyF9 = 0x43
        k KeyF10 = 0x44
        k KeyNumLock = 0x45
        k KeyPad7 = 0x47
        k KeyPad8 = 0x48
        k KeyPad9 = 0x49
        k KeyPad4 = 0x4B
        k KeyPad5 = 0x4C
        k KeyPad6 = 0x4D
        k KeyPad1 = 0x4F
        k KeyPad2 = 0x50
        k KeyPad3 = 0x51
        k KeyPad0 = 0x52
        k KeyF11 = 0x57
        k KeyF12 = 0x58
        k KeyF13 = 0x64
        k KeyF14 = 0x65
        k KeyF15 = 0x66
        k KeyHome = 0xC7
        k KeyUp = 0xC8
        k KeyLeft = 0xCB
        k KeyRight = 0xCD
        k KeyEnd = 0xCF
        k KeyDown = 0xD0
        k KeyInsert = 0xD2
        k KeyDelete = 0xD3