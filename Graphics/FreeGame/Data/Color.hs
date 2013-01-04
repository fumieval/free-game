-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.FreeGame.Data.Color
-- Copyright   :  (C) 2012 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinsohita <fumiexcel@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
-- Abstract structures that represents user interfaces
----------------------------------------------------------------------------
module Graphics.FreeGame.Data.Color (Color, fromRGBA, asWord8) where

import Data.Word

data Color = Color Float Float Float Float

fromRGBA :: Float -> Float -> Float -> Float -> Color
fromRGBA = Color

asWord8 :: Color -> (Word8, Word8, Word8, Word8)
asWord8 (Color r g b a) = (floor $ r * 255, floor $ g * 255, floor $ b * 255, floor $ a * 255)