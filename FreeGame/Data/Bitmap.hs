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
    ,bitmapSize
    ,liftBitmapIO
    -- * Load and Save
    ,readBitmap
    ,writeBitmap

    -- * Bitmap operations
    ,cropBitmap
    ,clipBitmap
    -- * V2
    ,sizeBitmap
    ) where

import qualified Codec.Picture as C
import qualified Codec.Picture.RGBA8 as C
import Control.Monad.IO.Class
import Data.BoundingBox
import Linear.V2
import System.Random
import Data.Hashable

data Bitmap = Bitmap { bitmapImage :: C.Image C.PixelRGBA8, bitmapHash :: Int }

-- | Get the size of the 'Bitmap'.
bitmapSize :: Bitmap -> (Int, Int)
bitmapSize (Bitmap (C.Image w h _) _) = (w, h)

liftBitmapIO :: MonadIO m => C.Image C.PixelRGBA8 -> m Bitmap
liftBitmapIO b = liftIO $ Bitmap b <$> randomIO

-- | Load an image file.
readBitmap :: MonadIO m => FilePath -> m Bitmap
readBitmap path = liftIO $ Bitmap <$> C.readImageRGBA8 path <*> randomIO

-- | Save 'Bitmap' into a file.
writeBitmap :: MonadIO m => FilePath -> Bitmap -> m ()
writeBitmap path (Bitmap p _) = liftIO $ C.writePng path p

-- | Extract a 'Bitmap' from the specified range.
cropBitmap :: Bitmap -- ^original bitmap
    -> (Int, Int) -- ^width and height
    -> (Int, Int) -- ^x and y
    -> Bitmap -- ^result
cropBitmap (Bitmap b k) (w, h) (x, y) = Bitmap
    (C.trimImage b (w, h) (x, y))
    (hash (w, h, x, y, k))

clipBitmap :: Bitmap -> Box V2 Int -> Bitmap
clipBitmap (Bitmap b k) (Box (V2 x0 y0) (V2 x1 y1)) = Bitmap
    (C.trimImage b (x1 - x0, y1 - y0) (x0, y0))
    (hash (x0, y0, x1, y1, k))

sizeBitmap :: Bitmap -> V2 Int
sizeBitmap (Bitmap (C.Image w h _) _) = V2 w h