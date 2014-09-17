{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  FreeGame
-- Copyright   :  (C) 2013 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
----------------------------------------------------------------------------
module FreeGame
  ( -- * System
    Time,
    System,
    MonadSystem(..),
    Control,
    Component(..),
    runGame,
    runGameDefault,
    WindowMode(..),
    BoundingBox2,
    Box(..),
    isInside,
    Picture(..),
    Bitmap,
    Affine(..),
    Picture2D(..),
    -- * IO
    liftIO,
    randomness,
    -- * Utility functions
    unitV2,
    angleV2,
    degrees,
    radians,
    -- * Reexports
    module Control.Monad,
    module Control.Applicative,
    module Control.Bool,
    module Data.Color,
    module Data.Color.Names,
    module Linear,

) where

import FreeGame.Util
import FreeGame.Types
import FreeGame.Component
import FreeGame.Data.Bitmap
import FreeGame.Picture
import FreeGame.Backend.GLFW
import Control.Monad.IO.Class
import Control.Monad
import Control.Applicative
import Control.Bool
import Data.Color
import Data.Color.Names
import Linear
import Data.BoundingBox

runGameDefault :: (forall s. System s a) -> IO ()
runGameDefault = runGame Windowed (Box (V2 0 0) (V2 640 480))
