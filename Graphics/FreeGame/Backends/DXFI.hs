{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.FreeGame.Backends.DXFI (runGame) where
import Graphics.FreeGame.Base
import Graphics.FreeGame.Bitmap
import Graphics.FreeGame.Input
import Graphics.FreeGame.Sound
import Control.Concurrent.ParallelIO.Local
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Free
import Data.Bits
import Data.Unique
import Data.Word
import Data.Array.Repa
import qualified Data.IntMap as IM
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Storable
import Codec.Picture.Repa
import System.FilePath.Windows
import System.Random
import GHC.Conc

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
foreign import ccall "DXFI_SetWindowSize" dxfi_SetWindowSize :: Int -> Int -> IO ()
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
foreign import ccall "DXFI_DeleteImage" dxfi_DeleteImage :: Handle -> IO ()
foreign import ccall "DXFI_DeleteSound" dxfi_DeleteSound :: Handle -> IO ()
foreign import ccall "DXFI_GetMouseInput" dxfi_GetMouseInput :: IO Int
foreign import ccall "DXFI_GetMouseWheelRotVol" dxfi_GetMouseWheelRotVol :: IO Int
foreign import ccall "DXFI_GetMousePoint" dxfi_GetMousePoint :: Ptr CInt -> Ptr CInt -> IO ()

loadImage :: Bitmap -> IO Handle
loadImage bmp = do
    let img = bitmapData bmp
    let Z :. height :. width :. _ = extent img
    bimg <- dxfi_Helper_SizeOfBaseImage >>= mallocBytes
    dxfi_CreateARGB8ColorBaseImage width height bimg
    withPool numCapabilities $ \pool -> parallel_ pool [
        forM_ [0..width-1] $ \x ->
            let r = (img !) $ Z :. y :. x :. 3
                g = (img !) $ Z :. y :. x :. 2
                b = (img !) $ Z :. y :. x :. 1
                a = (img !) $ Z :. y :. x :. 0
                in dxfi_SetPixelBaseImage bimg x y r g b a
        | y <- [0..height-1]]
    h <- dxfi_CreateGraphFromBaseImage bimg
    dxfi_ReleaseBaseImage bimg
    free bimg
    return h

unloadImage :: Handle -> IO ()
unloadImage = dxfi_DeleteImage

drawPicture' :: IM.IntMap Handle -> Picture -> IO ()
drawPicture' m picture = forM_ (trans picture) $ \(_, (x, y), s, a, h) -> dxfi_DrawImageBy (floor x) (floor y) s a h True False where
    trans (Image u) = [(False, (0,0), 1, 0, m IM.! hashUnique u)]
    trans (Transform pic) = [(True, (x, y), s, a, h) | (_, (x, y), s, a, h) <- trans pic]
    trans (NoTransform pic) = [(False, (x, y), s, a, h) | (_, (x, y), s, a, h) <- trans pic]
    trans (Translate (dx, dy) pic) = [(f, (x + dx, y + dy), s, a, h) | (f, (x, y), s, a, h) <- trans pic]
    trans (Rotate t pic) = [(f, (x * cos t - y * sin t, x * sin t + y * cos t), s, if f then a + t else a, h) | (f, (x, y), s, a, h) <- trans pic]
    trans (Scale k pic) = [(f, (k * x, k * y), if f then k * s else s, a, h) | (f, (x, y), s, a, h) <- trans pic]
    trans (Pictures ps) = concatMap trans ps
    
loadSound' :: FilePath -> IO Handle
loadSound' path = withCWString path $ \x -> dxfi_LoadSound x 3 (-1)

playSound' :: IM.IntMap Handle -> Sound -> IO ()
playSound' m (Wave u) = do
    let h = m IM.! hashUnique u
    dxfi_PlaySound h 1 True

unloadSound :: Handle -> IO ()
unloadSound = dxfi_DeleteSound

data SystemState = SystemState
    {
        sysLoadedImages :: IM.IntMap Handle
        ,sysLoadedSounds :: IM.IntMap Handle
        ,sysStartTime :: Int
        ,sysFrameCount :: Int
        ,sysRandomGen :: StdGen
        ,sysTimeCriteria :: Int
    }

runGame :: GameParam
    -> Game a -- ^the computation
    -> IO (Maybe a) -- ^result
runGame param game = do
    dxfi_SetWindowMode (windowed param)
    uncurry dxfi_SetWindowSize (windowSize param)
    dxfi_SetLogging False
    withCWString (windowTitle param) dxfi_SetWindowCaption
    
    dxfi_Initialize
    dxfi_SetDrawingDestination (-2)
    
    time <- dxfi_GetTickCount False
    gen <- maybe getStdGen (return . mkStdGen) $ randomSeed param
    result <- run [] [] game `evalStateT` SystemState IM.empty IM.empty time 0 gen time
    
    dxfi_Release
    
    return result
    where        
        run :: [Int] -> [Int] -> Game a -> StateT SystemState IO (Maybe a)
        run is ss (Pure a) = do
            st <- get
            lift $ forM_ is $ unloadImage . (sysLoadedImages st IM.!)
            lift $ forM_ ss $ unloadSound . (sysLoadedSounds st IM.!)
            put $ st { sysLoadedImages = foldr IM.delete (sysLoadedImages st) is
                     , sysLoadedSounds = foldr IM.delete (sysLoadedSounds st) ss }
            return (Just a)
        
        run is ss (Free x) = case x of
        
            DrawPicture pic cont -> do
                st <- get
                lift $ drawPicture' (sysLoadedImages st) pic
                run is ss cont
            
            PlaySound sound cont -> do
                st <- get
                lift $ playSound' (sysLoadedSounds st) sound
                run is ss cont
            
            AskInput key cont -> lift (isPressed key) >>= run is ss . cont
            
            Randomness r cont -> do
                st <- get
                let (v, g) = randomR r (sysRandomGen st)
                put $ st {sysRandomGen = g}
                run is ss $ cont v
            
            LoadPicture path cont -> do
                st <- get
                h <- lift $ loadImage path
                u <- lift $ newUnique
                put $ st { sysLoadedImages = IM.insert (hashUnique u) h (sysLoadedImages st) }
                run (hashUnique u:is) ss $ cont (Image u)
            
            LoadSound path cont -> do
                st <- get
                h <- lift $ loadSound' path
                u <- lift $ newUnique
                put $ st { sysLoadedSounds = IM.insert (hashUnique u) h (sysLoadedSounds st) }
                run is (hashUnique u:ss) $ cont (Wave u)
            
            GetMouseState cont -> do
                (x, y) <- lift $ alloca $ \px -> alloca $ \py ->
                    dxfi_GetMousePoint px py >> (,) `liftM` peek px `ap` peek py
                b <- lift $ dxfi_GetMouseInput
                w <- lift $ dxfi_GetMouseWheelRotVol
                run is ss $ cont $ MouseState (fromIntegral x, fromIntegral y) (testBit b 0) (testBit b 2) (testBit b 1) w

            EmbedIO m -> lift m >>= run is ss
            
            Bracket m -> run is ss m >>= maybe (return Nothing) (run is ss)
            
            GetRealTime cont -> lift (dxfi_GetTickCount False) >>= run is ss . cont . (/1000) . fromIntegral

            ResetRealTime cont -> do
                st <- get
                t <- lift $ dxfi_GetTickCount False
                put $ st { sysTimeCriteria = t }
                run is ss cont

            Tick cont -> do
                lift dxfi_FlipScreen
                
                time <- lift $ dxfi_GetTickCount False
                st@(SystemState _ _ startTime frameCount _ _) <- get
                lift $ dxfi_Wait $ (frameCount * 1000) `div` framePerSecond param - time + startTime
                
                if time - startTime >= 1000
                    then put $ st {sysStartTime　= time, sysFrameCount = 0}
                    else put $ st {sysFrameCount = succ frameCount}
                
                p <- lift processMessage

                if (p == 0)
                    then pre >> run is ss cont
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