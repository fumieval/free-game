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
import qualified Graphics.FreeGame.Backends.DXFI as DXFI
#endif


-- | Run the 'Game'.
runGame :: GameParam     
    -> Game a
    -> IO (Maybe a)

#ifdef mingw32_HOST_OS
runGame = DXFI.runGame
#else
runGame = undefined
#endif