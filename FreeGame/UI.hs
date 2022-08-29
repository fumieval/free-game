{-# LANGUAGE DeriveFunctor, ExistentialQuantification, Rank2Types, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  FreeGame.UI
-- Copyright   :  (C) 2013 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
-- Provides the "free" embodiment.
----------------------------------------------------------------------------
module FreeGame.UI (
    FreeGame(..)
) where

import FreeGame.Class
import FreeGame.Types
import FreeGame.Data.Bitmap (Bitmap)
import Data.Color

class (Picture2D m, Local m, Keyboard m, Mouse m) => FreeGame m where
    -- | Load a 'Bitmap' to avoid the cost of the first invocation of 'bitmap'.
    preloadBitmap :: Bitmap -> m ()
    -- | Generate a 'Bitmap' from the front buffer.
    takeScreenshot :: m Bitmap
    -- | Set the goal FPS.
    setFPS :: Double -> m ()
    setTitle :: String -> m ()
    showCursor :: m ()
    hideCursor :: m ()
    clearColor :: Color Float -> m ()
    -- | Get the actual FPS value.
    getFPS :: m Int
    getBoundingBox :: m BoundingBox2
    setBoundingBox :: BoundingBox2 -> m ()
