-----------------------------------------------------------------------------
-- |
-- Module      :  FreeGame.Data.Bitmap
-- Copyright   :  (C) 2013 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Manipulating bitmaps
----------------------------------------------------------------------------

module FreeGame.Data.Bitmap (
    -- * Basic types and functions
    Bitmap(..)
    , _BitmapArray
    , _BitmapHash
    ,bitmapSize

    -- * Load and Save
    ,readBitmap
    ,writeBitmap

    -- * Constructing bitmaps
    ,toBitmap
    ,toStableBitmap
    ,makeStableBitmap

    -- * Bitmap operations
    ,onBitmapWithHashable
    ,cropBitmap
    
    ) where

import Control.Applicative
import Codec.Picture.Repa
import qualified Codec.Picture as C
import Data.Array.Repa as R
import qualified Data.Array.Repa.Repr.ForeignPtr as RF
import Data.Word
import System.Random
import Data.Hashable
import Control.Monad.IO.Class
import qualified Data.Vector.Storable as V

-- | Bitmap data with unique hashes. 
data Bitmap = BitmapData (R.Array RF.F DIM3 Word8) (Maybe Int) -- ^ This value is used to ensure that two bitmaps are equivalent.

instance Show Bitmap where
    show (BitmapData _ h) = "<BitmapData #" Prelude.++ show h Prelude.++ ">"

instance Eq Bitmap where
    BitmapData _ h == BitmapData _ h' = h == h'

instance Ord Bitmap where
    BitmapData _ h <= BitmapData _ h' = h <= h'

-- | @'_BitmapArray' :: Lens' 'Bitmap' ('R.Array' 'RF.F' 'DIM3' 'Word8')@
-- The concrete data is stored as a repa array (y * x * ABGR).
_BitmapArray :: Functor f => (R.Array RF.F DIM3 Word8 -> f (R.Array RF.F DIM3 Word8)) -> Bitmap -> f Bitmap
_BitmapArray f (BitmapData a h) = fmap (\a' -> BitmapData a' h) (f a)

-- | @'_BitmapHash' :: Lens' 'Bitmap' ('Maybe' 'Int')@
_BitmapHash :: Functor f => (Maybe Int -> f (Maybe Int)) -> Bitmap -> f Bitmap
_BitmapHash f (BitmapData a h) = fmap (\h' -> BitmapData a h') (f h)

-- | Create unstable 'Bitmap' from the given array.
toBitmap :: R.Array RF.F DIM3 Word8 -> Bitmap
toBitmap ar = BitmapData ar Nothing

-- | Create stable 'Bitmap' from the given array and compute the hash.
toStableBitmap :: R.Array RF.F DIM3 Word8 -> Bitmap
toStableBitmap ar = BitmapData ar $ Just $ foldAllS combine 0 $ R.map fromIntegral ar where
    combine p q = hash (p, q)

-- | Create stable 'Bitmap' with unique hash from the given array.
makeStableBitmap :: R.Array RF.F DIM3 Word8 -> IO Bitmap
makeStableBitmap ar = BitmapData ar <$> Just <$> randomIO

-- | Get the size of the 'Bitmap'.
bitmapSize :: Bitmap -> (Int, Int)
bitmapSize (BitmapData a _) = let (Z :. h :. w :. _) = R.extent a in (w, h)

-- | Load an image file.
readBitmap :: MonadIO m => FilePath -> m Bitmap
readBitmap path = liftIO $ readImageRGBA path >>= either fail return >>= makeStableBitmap . imgData

-- | Save 'Bitmap' into a file.
writeBitmap :: MonadIO m => FilePath -> Bitmap -> m ()
writeBitmap path (BitmapData img _) = liftIO $ C.writePng path (C.Image w h $ V.unsafeFromForeignPtr0 (RF.toForeignPtr img) (h * w * 4) :: C.Image C.PixelRGBA8) where
    e@(R.Z R.:. h R.:. w R.:. z) = R.extent img

-- | Convert the 'Bitmap' uniformalized by the 'Hashable' value by the given function.
onBitmapWithHashable :: Hashable h => h -> (R.Array RF.F DIM3 Word8 -> R.Array RF.F DIM3 Word8) -> Bitmap -> Bitmap
onBitmapWithHashable v f (BitmapData ar h) = BitmapData (f ar) (hash <$> (,) v <$> h)

-- | Extract a 'Bitmap' from the specified range.
cropBitmap :: Bitmap -- ^original bitmap
    -> (Int, Int) -- ^width and height
    -> (Int, Int) -- ^x and y
    -> Bitmap -- ^result
cropBitmap bmp (w, h) (x, y) = onBitmapWithHashable (w,h,x,y) (computeS . extract (Z :. y :. x :. 0) (Z :. h :. w :. 4)) bmp
