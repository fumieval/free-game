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
    Bitmap
    ,bitmapSize

    -- * Load and Save
    ,readBitmap
    ,writeBitmap
    ,loadBitmapFromFile

    -- * Bitmap operations
    ,cropBitmap
    
    ) where

import qualified Codec.Picture as C
import qualified Codec.Picture.RGBA8 as C
import Control.Monad.IO.Class

type Bitmap = C.Image C.PixelRGBA8

-- | Get the size of the 'Bitmap'.
bitmapSize :: Bitmap -> (Int, Int)
bitmapSize (C.Image w h _) = (w, h)

-- | Load an image file.
readBitmap :: MonadIO m => FilePath -> m Bitmap
readBitmap path = liftIO (C.readImageRGBA8 path)

{-# DEPRECATED loadBitmapFromFile "use readBitmap instead" #-}
loadBitmapFromFile :: MonadIO m => FilePath -> m Bitmap
loadBitmapFromFile = readBitmap

-- | Save 'Bitmap' into a file.
writeBitmap :: MonadIO m => FilePath -> Bitmap -> m ()
writeBitmap path = liftIO . C.writePng path

-- | Extract a 'Bitmap' from the specified range.
cropBitmap :: Bitmap -- ^original bitmap
    -> (Int, Int) -- ^width and height
    -> (Int, Int) -- ^x and y
    -> Bitmap -- ^result
cropBitmap = C.trimImage