{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.FreeGame.Util
-- Copyright   :  (C) 2013 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinsohita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------

module Graphics.FreeGame.Util (untickGame, randomness, degrees, radians, loadPictureFromFile) where
import Control.Monad.Free
import Graphics.FreeGame.Base
import Graphics.FreeGame.Data.Bitmap
import System.Random

-- | Run a 'Game' as one frame.
untickGame :: Free GameAction a -> Free GameAction (Free GameAction a)
untickGame (Pure a) = Pure (Pure a)
untickGame (Free (Tick cont)) = Pure cont
untickGame (Free fm) = Free $ fmap untickGame fm

-- | Get a given range of value.
randomness :: (Random r, MonadFree GameAction m) => (r, r) -> m r
randomness r = embedIO $ randomRIO r

-- | Convert radians to degrees.
degrees :: Float -> Float
{-# INLINE degrees #-}
degrees x = x / pi * 180

-- | Convert degrees to radians.
radians :: Float -> Float
{-# INLINE radians #-}
radians x = x / 180 * pi

-- | Create a 'Picture' from the given file.
loadPictureFromFile :: MonadFree GameAction m => FilePath -> m Picture
loadPictureFromFile = embedIO . fmap BitmapPicture . loadBitmapFromFile