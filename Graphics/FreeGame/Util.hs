-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.FreeGame.Util
-- Copyright   :  (C) 2012 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinsohita <fumiexcel@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Transforming Game monads
----------------------------------------------------------------------------

module Graphics.FreeGame.Util where
import Control.Monad.Free
import qualified Control.Monad.Trans.Free as T
import Graphics.FreeGame.Base

-- | Run 'Game' as one frame.
untickGame :: Game a -> Game (Game a)
untickGame (Pure a) = Pure (Pure a)
untickGame (Free (Tick cont)) = Pure cont
untickGame (Free fm) = Free $ fmap untickGame fm