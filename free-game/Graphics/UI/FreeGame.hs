{-# LANGUAGE Rank2Types, FlexibleContexts #-}
{-|
Module      :  Graphics.UI.FreeGame
Copyright   :  (C) 2013 Fumiaki Kinoshita
License     :  BSD-style (see the file LICENSE)
Maintainer  :  Fumiaki Kinsohita <fumiexcel@gmail.com>

free-game is a library that abstracts and purifies GUI applications with simple interfaces.
-}
module Graphics.UI.FreeGame
  ( -- * Examples
    -- $example

    -- * Main
    Game,
    runGame,
    runSimple,
    def,
    
    -- * Reexports
    module Graphics.UI.FreeGame.Base,
    module Graphics.UI.FreeGame.Data.Bitmap,
    module Graphics.UI.FreeGame.Data.Font,
    module Graphics.UI.FreeGame.Data.Color,
    module Graphics.UI.FreeGame.GUI,
    module Graphics.UI.FreeGame.Util,
    module Graphics.UI.FreeGame.Types

) where

import Graphics.UI.FreeGame.Base
import Graphics.UI.FreeGame.GUI (GUI, GUIParam(..), Picture)
import Graphics.UI.FreeGame.Util
import Graphics.UI.FreeGame.Types
import Graphics.UI.FreeGame.Data.Bitmap
import Graphics.UI.FreeGame.Data.Font
import Graphics.UI.FreeGame.Data.Color
import qualified Graphics.UI.FreeGame.GUI.GLFW as GLFW
import Control.Monad.Free.Church
import Data.Default

-- | This is a limited form (and also extended form) of 'IO' monad.
type Game = F GUI

-- | Run a 'Game'.
runGame :: GUIParam -> Game a -> IO (Maybe a)
runGame = GLFW.runGame

-- | Run a 'Game' with modifying function
runSimple :: GUIParam
    -> world -- ^ An initial world
    -> (world -> Game world) -- ^ A computation yielding new world
    -> IO ()
runSimple param initial m = fmap (const ()) $ runGame param $ looping initial where
    looping world = do
        world' <- m world
        tick
        looping world'

{- $example

> import Graphics.UI.FreeGame
> main = runSimple def () return

shows a window and does nothing.

for more examples, see <https://github.com/fumieval/free-game/tree/master/examples>.

-}
