{-|
Module      :  Graphics.FreeGame
Copyright   :  (C) 2012 Fumiaki Kinoshita
License     :  BSD-style (see the file LICENSE)
Maintainer  :  Fumiaki Kinsohita <fumiexcel@gmail.com>

free-game is a library that abstracts and purifies GUI applications.

Small instruction:

    * load images by 'loadPictureFromFile'.

    * describe an application using 'drawPicture', 'askInput', 'tick', and so on, in Game monad.

    * apply 'runGame defaultGameParam' to run.

    * That's all!
-}
module Graphics.FreeGame
  ( -- * Examples
    -- $example

    -- * Note
    -- $note

    -- * Reexports
    module Graphics.FreeGame.Base,
    module Graphics.FreeGame.Bitmap,
    module Graphics.FreeGame.Input,
    module Graphics.FreeGame.Util,
    runGame
) where

import Graphics.FreeGame.Base
import Graphics.FreeGame.Bitmap
import Graphics.FreeGame.Input
import Graphics.FreeGame.Util
import Graphics.FreeGame.Backends.GLFW

{- $example

> main = runGame defaultGameParam $ forever tick

shows a window and does nothing.

for more examples, see <https://github.com/fumieval/free-game/tree/master/examples>.

-}

{- $note

* There are experimental implementation of text rendering('withRenderString'), but it often yields strange pictures.

-}
