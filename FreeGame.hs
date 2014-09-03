{-# OPTIONS_GHC -fno-warn-orphans #-}
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
  ( -- * Game
    System,
    runGame,
    runGameDefault,
    reGame,
    WindowMode(..),
    BoundingBox2,
    Box(..),
    isInside,
    stand,
    wait,
    -- * IO
    FromFinalizer(),
    embedIO,
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

import FreeGame.UI
import FreeGame.Util
import FreeGame.Types
import FreeGame.Text
import FreeGame.Core
import FreeGame.Class
import FreeGame.Instances ()
import FreeGame.Data.Bitmap
import FreeGame.Data.Font
import qualified FreeGame.Backend.GLFW as GLFW
import Control.Monad.IO.Class
import Control.Monad
import Control.Applicative
import Control.Bool
import Data.Color
import Data.Color.Names
import Linear
import Data.BoundingBox
import Control.Monad.Trans.Iter

-- | 'Game' is a kind of procedure but you can also use it like a value.
-- free-game's design is based on free structures, however, you don't have to mind it -- Just apply 'runGame', and enjoy.
--
-- <<http://shared.botis.org/free-game.png>>
--
-- For more examples, see <https://github.com/fumieval/free-game/tree/master/examples>.

runGame :: WindowMode -> BoundingBox2 -> System a -> IO ()
runGame = GLFW.runGame

runGameDefault :: System a -> IO ()
runGameDefault = runGame Windowed (Box (V2 0 0) (V2 640 480))

instance MonadIO Frame where
    liftIO = embedIO
