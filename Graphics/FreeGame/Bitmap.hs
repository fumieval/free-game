-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.FreeGame.Bitmap
-- Copyright   :  (C) 2012 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinsohita <fumiexcel@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Manipulating bitmaps
----------------------------------------------------------------------------

module Graphics.FreeGame.Bitmap (Bitmap, bitmapData, loadBitmapFromFile, cropBitmap) where

import Control.Applicative
import Codec.Picture.Repa
import Data.Array.Repa as R
import Data.Word
import qualified Data.Array.Repa.Repr.ForeignPtr as RF
import Data.Array.IArray as A
import qualified Graphics.Rendering.TrueType.STB as TT

newtype Bitmap = Bitmap { bitmapData :: R.Array D DIM3 Word8 }

loadBitmapFromFile :: FilePath -> IO Bitmap
loadBitmapFromFile path = Bitmap <$> delay <$> imgData <$> either error id <$> (readImageRGBA path)

cropBitmap :: Bitmap -> (Int, Int) -> (Int, Int) -> Bitmap
cropBitmap (Bitmap img) (w, h) (x, y) = Bitmap $ extract (Z :. y :. x :. 0) (Z :. h :. w :. 4) img

newtype Font = Font TT.BitmapCache

loadFontFromFile :: FilePath -> (Float, Float) -> IO Font
loadFontFromFile path sc = do
    tt <- TT.loadTTF path
    o <- head <$> TT.enumerateFonts tt
    font <- TT.initFont tt o
    Font <$> TT.newBitmapCache font False sc

charToBitmap :: Font -> Char -> IO (Maybe (Bitmap, Float))
charToBitmap (Font cache) ch = do
    r <- TT.getCachedBitmap cache ch
    case r of
        Just (TT.CBM bmp@(TT.Bitmap (w,h) _) _ (TT.HMetrics adv _)) -> do
            ar <- TT.bitmapArray bmp
            return $ Just (Bitmap $ fromFunction (Z :. h :. w :. 4) (\(Z:.y:.x:._) -> ar A.! (y, x)), adv)
        Nothing -> return Nothing