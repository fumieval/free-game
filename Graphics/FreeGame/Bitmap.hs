-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.FreeGame.Bitmap
-- Copyright   :  (C) 2012 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinsohita <fumiexcel@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
-- Manipulating bitmaps
----------------------------------------------------------------------------

module Graphics.FreeGame.Bitmap (Bitmap(..), bitmapSize, loadBitmapFromFile, cropBitmap, Font, loadFont, charToBitmap) where

import Control.Applicative
import Codec.Picture.Repa
import Data.Array.Repa as R
import Data.Word
import Data.Array.IArray as A
import qualified Graphics.Rendering.TrueType.STB as TT
import Graphics.FreeGame.Data.Color

-- | Concrete bitmap data
newtype Bitmap = Bitmap {
    bitmapData :: R.Array D DIM3 Word8 -- ^ Bare the 'Bitmap''s internal representation (y * x * RGBA)
    }

-- | Get the size of the 'Bitmap'.
bitmapSize :: Bitmap -> (Int, Int)
bitmapSize bmp = let (Z :. h :. w :. _) = R.extent (bitmapData bmp) in (w, h)

-- | Create a 'Bitmap' from the given file.
loadBitmapFromFile :: FilePath -> IO Bitmap
loadBitmapFromFile path = Bitmap <$> delay <$> imgData <$> either error id <$> readImageRGBA path

-- | Extract a 'Bitmap' from the specified range.
cropBitmap :: Bitmap -- ^original bitmap
    -> (Int, Int) -- ^width and height
    -> (Int, Int) -- ^x and y
    -> Bitmap -- ^result
cropBitmap (Bitmap img) (w, h) (x, y) = Bitmap $ extract (Z :. y :. x :. 0) (Z :. h :. w :. 4) img

-- | Font object
newtype Font = Font TT.BitmapCache

-- | Create a 'Font' from the given file.
loadFont :: FilePath -> Float -> IO Font
loadFont path size = do
    tt <- TT.loadTTF path
    o <- head <$> TT.enumerateFonts tt
    font <- TT.initFont tt o
    Just g <- TT.findGlyph font '|'
    TT.BBox (x0,y0) (x1,y1) <- TT.getGlyphBoundingBox font g
    let s = size/fromIntegral (x1-x0)
    Font <$> TT.newBitmapCache font False (s, s)

-- | Render 'Bitmap' of the character by specified 'Font' and color(RGB).
charToBitmap :: Font -> Color -> Char -> IO (Maybe (Bitmap, Float, Float, Float))
charToBitmap (Font cache) color ch = do
    let (red, green, blue, alpha) = asWord8 color
    r <- TT.getCachedBitmap cache ch
    case r of
        Just (TT.CBM bmp@(TT.Bitmap (w,h) _) (ox,oy) (TT.HMetrics adv _)) -> do
            ar <- TT.bitmapArray bmp
            let pixel (Z:.y:.x:.c) = [(ar A.! (y, x) * alpha) `div` 255, red, green, blue] !! c
            return $ Just (Bitmap $ fromFunction (Z :. h :. w :. 4) pixel, fromIntegral ox / 2, fromIntegral oy / 2, adv)
        Nothing -> return Nothing
