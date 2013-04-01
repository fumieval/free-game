{-# LANGUAGE Rank2Types, FlexibleContexts #-}
{-|
Module      :  Graphics.FreeGame
Copyright   :  (C) 2012 Fumiaki Kinoshita
License     :  BSD-style (see the file LICENSE)
Maintainer  :  Fumiaki Kinsohita <fumiexcel@gmail.com>

free-game is a library that abstracts and purifies GUI applications with simple interfaces.
-}
module Graphics.FreeGame
  ( -- * Examples
    -- $example

    -- * Reexports
    module Graphics.FreeGame.Base,
    module Graphics.FreeGame.Data.Bitmap,
    module Graphics.FreeGame.Data.Font,
    module Graphics.FreeGame.Data.Color,
    module Graphics.FreeGame.Input,
    module Graphics.FreeGame.Util,
    runGame,
    runGame'
) where

import Graphics.FreeGame.Base
import Graphics.FreeGame.Input
import Graphics.FreeGame.Util
import Graphics.FreeGame.Data.Bitmap
import Graphics.FreeGame.Data.Font
import Graphics.FreeGame.Data.Color
import qualified Graphics.FreeGame.Backends.GLFW as GLFW
import Control.Monad.Free

-- | Run a 'Game' computation.
runGame :: GameParam -> Game a -> IO (Maybe a)
runGame = GLFW.runGame

-- | In most cases there's no unwrapping (Game a -> GameAction (Game a)).
-- | The use of ('>>=') is more efficient than 'runGame' in such situation.
runGame' :: GameParam -> (forall m. MonadFree GameAction m => m a) -> IO (Maybe a)
runGame' = GLFW.runGame'

{- $example

> import Graphics.FreeGame.Simple
> main = runSimple defaultGameParam () return

shows a window and does nothing.

for more examples, see <https://github.com/fumieval/free-game/tree/master/examples>.

-}
