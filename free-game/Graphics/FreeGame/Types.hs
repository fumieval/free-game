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
module Graphics.FreeGame.Types (BoundingBox(..)) where
import Data.Vect
data BoundingBox = BoundingBox
    { topLeft :: Vec2
    , bottomRight :: Vec2
    } deriving (Show)