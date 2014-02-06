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
    WindowMode(..),
    BoundingBox(..),
    delay,
    tick,
    foreverFrame,
    untick,
    untickInfinite,
    -- * Frame
    Frame,
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
import Linear hiding (rotate)
import Control.Monad.Trans.Iter

-- | 'Game' is a monad literally expressing games.
-- This monad is an instance of 'Picture2D' so you can construct it using 'bitmap' and can be transformed with 'translate', 'scale', 'rotate', 'color'.
--
-- It is also an instance of 'Keyboard' and 'Mouse'. Note that 'mousePosition' returns a relative position.
--
-- > foo = foreverFrame $ do
-- >   p <- mousePosition
-- >   translate p $ color blue $ polygonOutline [V2 (-8) (-8), V2 8 (-8), V2 8 8, V2 (-8) 8]
-- 
-- When we run @foo@ using 'runGame', a blue square follows the cursor.
-- And 'translate' (V2 240 240) @foo@, 'rotate' 45 @foo@, 'scale' 1.5 @foo@ also does in the same way.
--
--
-- The only way to embody a 'Game' as a real stuff is to apply 'runGame'.
--
-- For more examples, see <https://github.com/fumieval/free-game/tree/master/examples>.

runGame :: WindowMode -> BoundingBox Double -> Game a -> IO (Maybe a)
runGame = GLFW.runGame

instance MonadIO Game where
    liftIO = embedIO

instance MonadIO Frame where
    liftIO = embedIO