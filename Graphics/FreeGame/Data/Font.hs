-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.FreeGame.Data.Font
-- Copyright   :  (C) 2012 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinsohita <fumiexcel@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
-- Rendering characters
----------------------------------------------------------------------------
module Graphics.FreeGame.Data.Font (Font, loadFont, RenderedChar(..), charToBitmap, withRenderString, withRenderCharacters ) where

import Control.Applicative
import Data.Array.Repa as R
import Data.Vect
import Data.Array.IArray as A
import qualified Graphics.Rendering.TrueType.STB as TT
import Graphics.FreeGame.Base
import Graphics.FreeGame.Data.Color
import Graphics.FreeGame.Data.Bitmap

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
    let s = 5 * size / fromIntegral (x1 - x0)
    Font <$> TT.newBitmapCache font False (s, s)

data RenderedChar = RenderedChar
    {
        charBitmap :: Bitmap
        ,charOffset :: Vec2
        ,charAdvance :: Float
    }

-- | Render 'Bitmap' of the character by specified 'Font' and color(RGB).
charToBitmap :: Font -> Color -> Char -> IO (Maybe RenderedChar)
charToBitmap (Font cache) color ch = do
    let (cr, cg, cb, ca) = colorAsWord8 color
    r <- TT.getCachedBitmap cache ch
    case r of
        Nothing -> return Nothing
        Just (TT.CBM bmp@(TT.Bitmap (w,h) _) (ox,oy) (TT.HMetrics adv _)) -> do
            ar <- TT.bitmapArray bmp
            let pixel (Z:.y:.x:.c) = [ar A.! (y + 1, x), cb, cg, cr] !! c
            bmp <- makeStableBitmap $ fromFunction (Z :. h - 1 :. w :. 4) pixel
            return $ Just $ RenderedChar bmp (Vec2 (fromIntegral ox / 2) (fromIntegral oy / 2)) adv


-- | Render the string by the given font and color, and pass it to the 'Game' computation. 
withRenderCharacters :: Font -> Color -> String -> ([Picture] -> Game a) -> Game a
withRenderCharacters font color str action = bracket $ render str 0 >>= action
    where
        render [] _ = return []
        render (c:cs) x = do
            Just (RenderedChar b (Vec2 o h) w) <- embedIO $ charToBitmap font color c
            (Translate (Vec2 (x + w + o) h) (BitmapPicture b):) <$> render cs (x + w)

withRenderString :: Font -> Color -> String -> (Picture -> Game a) -> Game a
withRenderString font color str action = withRenderCharacters font color str (action . Pictures)
