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
    -- * Color operations
    , transparent
    , lighten
    , darken
    , intermediate
    -- * Deprecated
    , halfD
    , halfB
    -- * Basic colors
    , white, black, red, green, blue, yellow, cyan, magenta
    ) where

import Data.String
import Data.Char

-- | A color that has red, green, blue, alpha as its component.
data Color = Color Float Float Float Float deriving (Show, Eq, Ord)

hf :: Char -> Float
hf x = fromIntegral (digitToInt x) / 15

hf' :: Char -> Char -> Float
hf' x y = fromIntegral (digitToInt x * 16 + digitToInt y) / 255

instance IsString Color where
    fromString xs@[r,g,b,a] | all isHexDigit xs = Color (hf r) (hf g) (hf b) (hf a)
    fromString xs@[r,g,b] | all isHexDigit xs = Color (hf r) (hf g) (hf b) 1
    fromString xs@[r1,r0,g1,g0,b1,b0,a1,a0] | all isHexDigit xs = Color (hf' r1 r0) (hf' g1 g0) (hf' b1 b0) (hf' a1 a0)
    fromString xs@[r1,r0,g1,g0,b1,b0] | all isHexDigit xs = Color (hf' r1 r0) (hf' g1 g0) (hf' b1 b0) 1
    fromString x = error $ "Invalid color representation: " ++ x

transparent :: Float -> Color -> Color
transparent f (Color r g b a) = Color r g b (f * a)

lighten :: Float -> Color -> Color
lighten f (Color r g b a) = Color (r * (1 - f) + f) (g * (1 - f) + f) (b * (1 - f) + f) a

darken :: Float -> Color -> Color
darken f (Color r g b a) = Color (r * (1 - f)) (g * (1 - f)) (b * (1 - f)) a

-- | An intermediate between the given colors.
intermediate :: Color -> Color -> Color
intermediate (Color r0 g0 b0 a0) (Color r1 g1 b1 a1) = Color ((r0 + r1)/2) ((g0 + g1)/2) ((b0 + b1)/2) ((a0 + a1)/2)

{-# DEPRECATED halfD "use darken 0.5 instead" #-}
-- | An intermediate between the black and the given color
halfD :: Color -> Color
halfD = intermediate black

{-# DEPRECATED halfB "use lighten 0.5 instead" #-}
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
