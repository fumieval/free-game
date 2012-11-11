{-# LANGUAGE FlexibleContexts #-}
module FreeGame.Util where
import Control.Applicative
import Control.Monad
import Control.Monad.Free
import FreeGame.Base
import FreeGame.Input
import FreeGame.Sound
import FreeGame.Bitmap
import System.Random

untick :: Free Game a -> Free Game (Free Game a)
untick (Pure a) = Pure (Pure a)
untick (Free (Tick cont)) = Pure cont
untick (Free f) = Free $ fmap untick f

tick :: MonadFree Game m => m ()
tick = wrap $ Tick (return ())

playSound :: MonadFree Game m => Sound -> m ()
playSound sound = wrap $ PlaySound sound (return ())

drawPicture :: MonadFree Game m => Picture -> m ()
drawPicture pic = wrap $ DrawPicture pic (return ())

loadPicture :: MonadFree Game m => Bitmap -> m Picture
loadPicture img = wrap $ LoadPicture img return

askInput :: MonadFree Game m => Key -> m Bool
askInput key = wrap $ AskInput key return

loadSound :: MonadFree Game m => FilePath -> m Sound
loadSound path = wrap $ LoadSound path return

close :: MonadFree Game m => Free Game a -> m a
close m = wrap $ Close $ liftM return m

embedIO :: MonadFree Game m => IO a -> m a
embedIO m = wrap $ EmbedIO $ liftM return m

randomness :: (Random r, MonadFree Game m) => (r,r) -> m r
randomness r = wrap $ Randomness r return
