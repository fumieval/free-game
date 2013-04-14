{-# LANGUAGE DeriveFunctor, Rank2Types, FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.FreeGame.Types
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
    ) where

import Linear hiding (rotate)

-- | FIXME: this should inherit more classes
data BoundingBox a = BoundingBox (V2 a) (V2 a) deriving (Show, Eq, Ord, Functor, Read)

_topLeft :: Functor f => (V2 a -> f (V2 a)) -> (BoundingBox a -> f (BoundingBox a))
_topLeft f (BoundingBox a b) = fmap (`BoundingBox` b) (f a)

_bottomRight :: Functor f => (V2 a -> f (V2 a)) -> (BoundingBox a -> f (BoundingBox a))
_bottomRight f (BoundingBox a b) = fmap (a `BoundingBox`) (f b)
