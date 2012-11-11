module FreeGame.Bitmap (Bitmap, bitmapData, loadBitmapFromFile, cropBitmap) where
import Codec.Picture.Repa
import Control.Applicative
import Data.Array.Repa as R
import Data.Vect.Double
import GHC.Float
import Data.Word
import qualified Data.Array.Repa.Repr.ForeignPtr as RF

newtype Bitmap = Bitmap (R.Array D DIM3 Word8)

bitmapData :: Bitmap -> R.Array D DIM3 Word8
bitmapData (Bitmap arr) = arr

loadBitmapFromFile :: FilePath -> IO Bitmap
loadBitmapFromFile path = Bitmap <$> delay <$> imgData <$> either error id <$> (readImageRGBA path)

cropBitmap :: Bitmap -> (Int, Int) -> (Int, Int) -> Bitmap
cropBitmap (Bitmap img) (w, h) (x, y) = Bitmap $ extract (Z :. y :. x :. 0) (Z :. h :. w :. 4) img
