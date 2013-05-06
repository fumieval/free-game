{-# LANGUAGE DeriveFunctor #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.FreeGame.Types
-- Copyright   :  (C) 2013 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinsohita <fumiexcel@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Graphics.UI.FreeGame.Types (
    BoundingBox(..),
    inBoundingBox,
    _TopLeft,
    _TopRight,
    _BottomLeft,
    _BottomRight
    ) where

import Linear.V2

-- | 2D bounding box.
data BoundingBox a = BoundingBox a a a a deriving (Show, Eq, Ord, Functor, Read)

-- | Determine whether the given point is in the 'BoundingBox'.
inBoundingBox :: Ord a => V2 a -> BoundingBox a -> Bool
inBoundingBox (V2 x y) (BoundingBox x0 y0 x1 y1) = x0 <= x && x <= x1 && y0 <= y && y <= y1

-- | @'_TopLeft' :: Lens' ('BoundingBox' a) ('V2' a)@
_TopLeft :: Functor f => (V2 a -> f (V2 a)) -> (BoundingBox a -> f (BoundingBox a))
_TopLeft f (BoundingBox x0 y0 x1 y1) = fmap (\(V2 x0' y0') -> BoundingBox x0' y0' x1 y1) (f (V2 x0 y0))

-- | @'_TopRight' :: Lens' ('BoundingBox' a) ('V2' a)@
_TopRight :: Functor f => (V2 a -> f (V2 a)) -> (BoundingBox a -> f (BoundingBox a))
_TopRight f (BoundingBox x0 y0 x1 y1) = fmap (\(V2 x1' y0') -> BoundingBox x0 y0' x1' y1) (f (V2 x1 y0))

-- | @'_BottomLeft' :: Lens' ('BoundingBox' a) ('V2' a)@
_BottomLeft :: Functor f => (V2 a -> f (V2 a)) -> (BoundingBox a -> f (BoundingBox a))
_BottomLeft f (BoundingBox x0 y0 x1 y1) = fmap (\(V2 x0' y1') -> BoundingBox x0' y0 x1 y1') (f (V2 x0 y1))

-- | @'_BottomRight' :: Lens' ('BoundingBox' a) ('V2' a)@
_BottomRight :: Functor f => (V2 a -> f (V2 a)) -> (BoundingBox a -> f (BoundingBox a))
_BottomRight f (BoundingBox x0 y0 x1 y1) = fmap (\(V2 x1' y1') -> BoundingBox x0 y0 x1' y1') (f (V2 x1 y1))
