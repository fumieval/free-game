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
module Graphics.FreeGame.Data.Color (Color, fromRGBA, colorAsWord8, white, black, red, green, blue, yellow, cyan, magenta, intermediate, halfD, halfB) where

import Data.Word

data Color = Color Float Float Float Float

fromRGBA :: Float -> Float -> Float -> Float -> Color
fromRGBA = Color

colorAsWord8 :: Color -> (Word8, Word8, Word8, Word8)
colorAsWord8 (Color r g b a) = (floor $ r * 255, floor $ g * 255, floor $ b * 255, floor $ a * 255)

intermediate :: Color -> Color -> Color
intermediate (Color r0 g0 b0 a0) (Color r1 g1 b1 a1) = Color ((r0 + r1)/2) ((g0 + g1)/2) ((b0 + b1)/2) ((a0 + a1)/2)

halfD :: Color -> Color
halfD = intermediate black

halfB :: Color -> Color
halfB = intermediate white

white :: Color
white = Color 1.0 1.0 1.0 1.0

black :: Color
black = Color 0.0 0.0 0.0 1.0

red :: Color
red = Color 1.0 0.0 0.0 1.0

green :: Color
green = Color 0.0 1.0 0.0 1.0

blue :: Color
blue = Color 0.0 0.0 1.0 1.0

yellow :: Color
yellow = Color 1.0 1.0 0.0 1.0

cyan :: Color
cyan = Color 0.0 1.0 1.0 1.0

magenta :: Color
magenta = Color 1.0 0.0 1.0 1.0
