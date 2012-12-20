{-# LANGUAGE CPP #-}
module Graphics.FreeGame (
    module Graphics.FreeGame.Base
    ,module Graphics.FreeGame.Bitmap
    ,module Graphics.FreeGame.Sound
    ,module Graphics.FreeGame.Input
    ,module Graphics.FreeGame.Util
    ,runGame
) where

import Graphics.FreeGame.Base
import Graphics.FreeGame.Bitmap
import Graphics.FreeGame.Sound
import Graphics.FreeGame.Input
import Graphics.FreeGame.Util

#ifdef mingw32_HOST_OS
import Graphics.FreeGame.Backends.DXFI
#else
runGame = undefined
#endif