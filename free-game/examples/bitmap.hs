import Graphics.FreeGame.Simple
import Graphics.FreeGame.Data.Bitmap
import Control.Monad
import Data.Array.Repa
import Data.Word

renderCircle :: Int -> (Word8, Word8, Word8, Word8) -> Bitmap
renderCircle size (r,g,b,a) = toStableBitmap $ computeS $ fromFunction (Z :. size :. size :. 4) render where
    center = fromIntegral size / 2
    render (Z:.y:.x:.0)
        | s < 0 = a
        | s >= 1 = 0
        | otherwise = floor ((1 - s) * 256)
        where
            r = sqrt $ (fromIntegral y - center) ^ 2 + (fromIntegral x - center) ^ 2
            s = r - fromIntegral size / 2
    render (Z:._:._:.c) = [undefined,b,g,r] !! c

main = runSimple defaultGameParam () $ \_ -> drawPicture $ Translate (Vec2 240 240) circle
    where
        circle = Scale (Vec2 0.5 0.5) $ BitmapPicture $ renderCircle 128 (128,216,128,255)