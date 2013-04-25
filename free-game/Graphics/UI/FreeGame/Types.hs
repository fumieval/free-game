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
    _TopLeft,
    _BottomRight
    ) where

import Linear hiding (rotate)

-- | FIXME: this should inherit more classes
data BoundingBox a = BoundingBox (V2 a) (V2 a) deriving (Show, Eq, Ord, Functor, Read)

_TopLeft :: Functor f => (V2 a -> f (V2 a)) -> (BoundingBox a -> f (BoundingBox a))
_TopLeft f (BoundingBox a b) = fmap (`BoundingBox` b) (f a)

_BottomRight :: Functor f => (V2 a -> f (V2 a)) -> (BoundingBox a -> f (BoundingBox a))
_BottomRight f (BoundingBox a b) = fmap (a `BoundingBox`) (f b)
