-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.FreeGame.Data.Color
-- Copyright   :  (C) 2013 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinsohita <fumiexcel@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
-- Colors and its operations
----------------------------------------------------------------------------
module Graphics.FreeGame.Data.Color (
    -- * The type
    Color(..)
    , colorAsWord8
    -- * Color operations
    , transparent
    , intermediate
    , halfD
    , halfB
    -- * Basic colors
    , white, black, red, green, blue, yellow, cyan, magenta
    ) where

import Data.Word

-- | A color that has red, green, blue, alpha as its component.
data Color = Color Float Float Float Float deriving (Show, Eq, Ord)

colorAsWord8 :: Color -> (Word8, Word8, Word8, Word8)
colorAsWord8 (Color r g b a) = (floor $ r * 255, floor $ g * 255, floor $ b * 255, floor $ a * 255)

-- | An intermediate between the given colors.
intermediate :: Color -> Color -> Color
intermediate (Color r0 g0 b0 a0) (Color r1 g1 b1 a1) = Color ((r0 + r1)/2) ((g0 + g1)/2) ((b0 + b1)/2) ((a0 + a1)/2)

-- | Give a transparency to the color.
transparent :: Float -> Color -> Color
transparent f (Color r g b a) = Color r g b (f * a)

-- | An intermediate between the black and the given color
halfD :: Color -> Color
halfD = intermediate black

-- | An intermediate between the white and the given color
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
