{-# LANGUAGE RankNTypes, FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.FreeGame.Simple
-- Copyright   :  (C) 2013 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinsohita <fumiexcel@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
-- The essentials of using free-game
----------------------------------------------------------------------------
module Graphics.FreeGame.Simple (
    -- * Basic type
    Game
    ,GameAction

    -- * Run the game
    ,GameParam(..)
    ,defaultGameParam
    ,runSimple
    ,runSimple'

    -- * In the Game monad
    ,drawPicture
    ,getButtonState
    ,getMousePosition
    ,getMouseWheel
    ,embedIO
    ,quitGame
    ,tick
    ,untick
    ,untickInfinite
    ,untickGame

    -- * About Picture
    ,Picture(..)
    ,Bitmap
    ,loadBitmapFromFile
    ,loadBitmaps
    ,loadBitmapsWith
    ,Vec2(..)

    -- * Drawing texts
    ,Font
    ,loadFont
    ,text

    -- * Utilities
    ,randomness
    ,degrees

    -- * Deprecated
    ,askInput
    ,getMouseState

    -- * Reexports
    ,module Graphics.FreeGame.Input
    ,module Graphics.FreeGame.Data.Color
    )

where

import Graphics.FreeGame
import Graphics.FreeGame.Data.Color
import Graphics.FreeGame.Input
import Control.Monad
import Control.Monad.Free

-- | Run a 'Game' by the given initial state and updating function.
runSimple :: GameParam
    -> world -- ^ An initial world
    -> (world -> Game world) -- ^ A computation yielding new world
    -> IO ()
runSimple param initial m = void $ runGame param $ looping initial where
    looping world = do
        world' <- m world
        tick
        looping world'

-- | In most cases there's no unwrapping (Game a -> GameAction (Game a)).
-- | The use of ('>>=') is more efficient than 'runGame' in such situation.
runSimple' :: GameParam
    -> world -- ^ An initial world
    -> (world -> forall m. MonadFree GameAction m => m world) -- ^ A computation yielding new world
    -> IO ()
runSimple' param initial m = void $ runGame' param $ looping initial where
    looping world = do
        world' <- m world
        tick
        looping world'