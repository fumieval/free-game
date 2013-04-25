{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.FreeGame.Data.Color
-- Copyright   :  (C) 2013 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinsohita <fumiexcel@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
-- Colors and its operations
----------------------------------------------------------------------------
module Graphics.UI.FreeGame.Data.Color (
    -- * The type
    Color(..)
    -- * Color operations
    , transparent
    , blend
    -- * Lenses
    , _Red, _Green, _Blue, _Alpha, _8Bit
    -- * Basic colors
    , white, black, red, green, blue, yellow, cyan, magenta
    ) where

import Data.String
import Data.Char
import Data.Profunctor
import Data.Word

-- | A color that has red, green, blue, alpha as its component.
data Color = Color Float Float Float Float deriving (Show, Eq, Ord)

_8Bit :: forall p f. (Profunctor p, Functor f) => p Word8 (f Word8) -> p Float (f Float)
_8Bit = dimap (floor.(*255)) (fmap ((/255) . fromIntegral))

_Red :: Functor f => (Float -> f Float) -> Color -> f Color
_Red f (Color r g b a) = fmap (\r' -> Color r' g b a) (f r)

_Green :: Functor f => (Float -> f Float) -> Color -> f Color
_Green f (Color r g b a) = fmap (\g' -> Color r g' b a) (f g)

_Blue :: Functor f => (Float -> f Float) -> Color -> f Color
_Blue f (Color r g b a) = fmap (\b' -> Color r g b' a) (f b)

_Alpha :: Functor f => (Float -> f Float) -> Color -> f Color
_Alpha f (Color r g b a) = fmap (\a' -> Color r g b a) (f a)
    
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

blend :: Float -> Color -> Color -> Color
blend t (Color r0 g0 b0 a0) (Color r1 g1 b1 a1) = Color
    (r0 * (1 - t) + r1 * t)
    (g0 * (1 - t) + g1 * t)
    (b0 * (1 - t) + b1 * t)
    (a0 * (1 - t) + a1 * t)

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
