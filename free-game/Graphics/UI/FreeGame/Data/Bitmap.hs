-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.FreeGame.Data.Bitmap
-- Copyright   :  (C) 2013 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinsohita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Manipulating bitmaps
----------------------------------------------------------------------------

module Graphics.UI.FreeGame.Data.Bitmap (
    -- * Basic types and functions
    Bitmap(..)
    ,bitmapSize

    -- * Loading from a file
    ,loadBitmapFromFile

    -- * Constructing bitmaps
    ,toBitmap
    ,toStableBitmap
    ,makeStableBitmap

    -- * Bitmap operations
    ,onBitmap
    ,onBitmapWithHashable
    ,cropBitmap
    
    ) where

import Control.Applicative
import Codec.Picture.Repa
import Data.Array.Repa as R
import qualified Data.Array.Repa.Repr.ForeignPtr as RF
import Data.Word
import System.Random
import Data.Hashable

-- | Concrete bitmap data
data Bitmap = BitmapData {
    bitmapData :: R.Array RF.F DIM3 Word8 -- ^ Bare the 'Bitmap''s internal representation (y * x * RGBA).
    ,bitmapHash :: Maybe Int -- ^ This value is used to ensure that two bitmaps are equivalent.
    }

-- | Create unstable 'Bitmap' from the given array.
toBitmap :: R.Array RF.F DIM3 Word8 -> Bitmap
toBitmap ar = BitmapData  ar Nothing

-- | Create stable 'Bitmap' from the given array and compute the hash.
toStableBitmap :: R.Array RF.F DIM3 Word8 -> Bitmap
toStableBitmap ar = BitmapData  ar $ Just $ head $ foldAllP combine 0 $ R.map fromIntegral ar where
    combine p q = hash (p, q)

-- | Create stable 'Bitmap' with unique hash from the given array.
makeStableBitmap :: R.Array RF.F DIM3 Word8 -> IO Bitmap
makeStableBitmap ar = BitmapData ar <$> Just <$> randomIO

-- | Get the size of the 'Bitmap'.
bitmapSize :: Bitmap -> (Int, Int)
bitmapSize bmp = let (Z :. h :. w :. _) = R.extent (bitmapData bmp) in (w, h)

-- | Create a 'Bitmap' from the given file.
loadBitmapFromFile :: FilePath -> IO Bitmap
loadBitmapFromFile path = readImageRGBA path >>= either fail return >>= makeStableBitmap . imgData

-- | Convert the 'Bitmap' by the given function.
onBitmap :: (R.Array RF.F DIM3 Word8 -> R.Array RF.F DIM3 Word8) -> Bitmap -> Bitmap
onBitmap f = toStableBitmap . f . bitmapData

-- | Convert the 'Bitmap' uniformalized by the 'Hashable' value by the given function.
onBitmapWithHashable :: Hashable h => h -> (R.Array RF.F DIM3 Word8 -> R.Array RF.F DIM3 Word8) -> Bitmap -> Bitmap
onBitmapWithHashable v f (BitmapData ar h) = BitmapData (f ar) (hash <$> (,) v <$> h)

-- | Extract a 'Bitmap' from the specified range.
cropBitmap :: Bitmap -- ^original bitmap
    -> (Int, Int) -- ^width and height
    -> (Int, Int) -- ^x and y
    -> Bitmap -- ^result
cropBitmap bmp (w, h) (x, y) = onBitmapWithHashable (w,h,x,y) (head . computeP . extract (Z :. y :. x :. 0) (Z :. h :. w :. 4)) bmp
