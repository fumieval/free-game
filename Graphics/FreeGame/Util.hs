{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.FreeGame.Util
-- Copyright   :  (C) 2012 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinsohita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------

module Graphics.FreeGame.Util (untickGame, randomness, degrees, radians, withRenderString, loadPictureFromFile) where
import Control.Applicative
import Control.Monad
import Control.Monad.Free
import qualified Control.Monad.Trans.Free as T
import Graphics.FreeGame.Base
import Graphics.FreeGame.Bitmap
import Graphics.FreeGame.Data.Color
import System.Random
import Data.Vect
import Data.Word

-- | Run a 'Game' as one frame.
untickGame :: Game a -> Game (Game a)
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

-- | Render the string by the given font and color, and pass it to the 'Game' computation. 
withRenderCharacters :: Font -> Color -> String -> ([Picture] -> Game a) -> Game a
withRenderCharacters font color str action = bracket $ render str 0 >>= action
    where
        render [] _ = return []
        render (c:cs) x = do
            Just (b, o, h, w) <- embedIO $ charToBitmap font color c
            (:) <$> Translate (Vec2 (x + w + o) h) <$> loadPicture b
                <*> render cs (x + w)

withRenderString :: Font -> Color -> String -> (Picture -> Game a) -> Game a
withRenderString font color str action = withRenderCharacters font color str (action . Pictures)

-- | Create a 'Picture' from the given file.
loadPictureFromFile :: FilePath -> Game Picture
loadPictureFromFile = embedIO . loadBitmapFromFile >=> loadPicture