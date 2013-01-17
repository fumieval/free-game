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

    -- * Run the game
    ,GameParam
    ,defaultGameParam
    ,runSimple

    -- * In the Game monad
    ,drawPicture
    ,askInput
    ,getMouseState
    ,embedIO
    ,quitGame
    ,tick
    ,untickGame

    -- * About Picture
    ,Picture(..)
    ,Bitmap
    ,loadBitmapFromFile
    ,Vec2(..)

    -- * Drawing texts
    ,Font
    ,loadFont
    ,text

    -- * Utilities
    ,randomness
    ,degrees

    -- * Reexports
    ,module Graphics.FreeGame.Input
    ,module Graphics.FreeGame.Data.Color
    )

where

import Graphics.FreeGame
import Graphics.FreeGame.Data.Color
import Graphics.FreeGame.Data.Font
import Graphics.FreeGame.Input
import Control.Monad

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