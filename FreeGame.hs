{-# LANGUAGE GeneralisedNewtypeDeriving #-}
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
    Game(..),
    runGame,
    runGameDefault,
    WindowMode(..),
    BoundingBox2,
    Box(..),
    isInside,
    delay,
    tick,
    foreverFrame,
    untick,
    untickInfinite,
    -- * Frame
    Frame,
    FreeGame(..),
    liftFrame,
    -- * Transformations
    Vec2,
    Affine(..),
    Local(),
    globalize,
    localize,
    -- * Pictures
    Picture2D(..),
    BlendMode(..),
    Bitmap,
    bitmapSize,
    readBitmap,
    cropBitmap,
    clipBitmap,
    loadBitmaps,
    loadBitmapsWith,
    writeBitmap,
    -- * Text
    Font,
    loadFont,
    text,
    -- * Keyboard
    Keyboard(..),
    Key(..),
    charToKey,
    keyPress,
    keyUp,
    keyDown,
    -- * Mouse
    Mouse(),
    mouseScroll,
    mouseInWindow,
    mousePositionMay,
    mousePosition,
    mouseButtonL,
    mouseButtonR,
    mouseButtonM,
    mouseDownL,
    mouseDownR,
    mouseDownM,
    mouseUpL,
    mouseUpR,
    mouseUpM,
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
    -- * Deprecated
    keyChar,
    keySpecial

) where

import Control.Applicative
import Control.Bool
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Data.BoundingBox
import Data.Color
import Data.Color.Names
import FreeGame.Backend.GLFW
import FreeGame.Class
import FreeGame.Data.Bitmap
import FreeGame.Data.Font
import FreeGame.Instances ()
import FreeGame.Text
import FreeGame.Types
import FreeGame.UI
import FreeGame.Util
import Linear

liftFrame :: Frame a -> Game a
liftFrame = Game . lift

runGameDefault :: Game a -> IO (Maybe a)
runGameDefault = runGame Windowed (Box (V2 0 0) (V2 640 480))
