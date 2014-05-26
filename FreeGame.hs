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
    Game,
    runGame,
    runGameDefault,
    reGame,
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
    reFrame,
    FreeGame(..),
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
    loadBitmaps,
    loadBitmapsWith,
    writeBitmap,
    -- * 3D
    Picture3D(..),
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
    -- * Deprecated
    fromBitmap,
    loadBitmapFromFile,
    colored,
    keyChar,
    keySpecial

) where

import FreeGame.UI
import FreeGame.Util
import FreeGame.Types
import FreeGame.Text
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

runGame :: WindowMode -> BoundingBox2 -> Game a -> IO (Maybe a)
runGame = GLFW.runGame

runGameDefault :: Game a -> IO (Maybe a)
runGameDefault = runGame Windowed (Box (V2 0 0) (V2 640 480))

instance MonadIO Frame where
    liftIO = embedIO
