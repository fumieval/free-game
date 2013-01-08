-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.FreeGame.Base
-- Copyright   :  (C) 2012 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinsohita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- The essentials of using free-game
----------------------------------------------------------------------------
module Graphics.FreeGame.Simple (
    -- * Basic type
    Game

    -- * Run the game
    ,defaultGameParam
    ,runSimple

    -- * In the Game monad
    ,drawPicture
    ,askInput
    ,getMouseState
    ,embedIO
    ,quitGame

    -- * About Picture
    ,Picture(..)
    ,loadBitmapFromFile
    ,Vec2(..)

    )

where

import Graphics.FreeGame
import Control.Monad

runSimple :: GameParam
    -> world -- ^ An initial world
    -> (world -> Game world) -- ^ A computation yielding new world
    -> IO ()
runSimple param initial m = void $ runGame param $ looping initial
    where
        looping world = do
            world' <- m world
            tick
            looping world'