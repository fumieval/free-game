{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  FreeGame.Types
-- Copyright   :  (C) 2013 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module FreeGame.Types (
    WindowMode(..)
    , Vec2
    , BoundingBox2
    , Key(..)
    , BlendMode(..)
    ) where

import Linear.V2
import Data.Typeable
import Data.BoundingBox
import Graphics.Holz.System (WindowMode(..))
import Graphics.Holz.Input

type Vec2 = V2 Double

type BoundingBox2 = Box V2 Double

data BlendMode = Normal
    | Inverse
    | Add
    | Multiply
    | Screen
    deriving (Enum, Eq, Ord, Read, Show, Typeable)
