{-# LANGUAGE FlexibleContexts #-}
module FreeGame.Util where
import Control.Monad.Free
import Control.Monad.Trans
import qualified Control.Monad.Trans.Free as T
import FreeGame.Base
import FreeGame.Graphic
import FreeGame.Sound
import FreeGame.Input
import Control.Monad
import Codec.Picture.Repa
import Data.Array.Repa
import Control.Monad.Identity
import qualified Data.Array.Repa.Repr.ForeignPtr as RF

untick (Pure a) = Pure (Pure a)
untick (Free (Tick cont)) = Pure cont
untick (Free f) = Free $ fmap untick f

tick :: MonadFree Game m => m ()
tick = wrap $ Tick (return ())

playSound :: MonadFree Game m => Sound -> m ()
playSound sound = wrap $ PlaySound sound (return ())

drawPicture :: MonadFree Game m => Picture -> m ()
drawPicture pic = wrap $ DrawPicture pic (return ())

loadImage :: MonadFree Game m => Img RGBA -> m Picture
loadImage img = wrap $ LoadImage img return

askInput :: MonadFree Game m => Key -> m Bool
askInput key = wrap $ AskInput key return

loadSound :: MonadFree Game m => FilePath -> m WaveData
loadSound path = wrap $ LoadSound path return

embedIO :: MonadFree Game m => IO a -> m a
embedIO m = wrap $ EmbedIO $ liftM return m

randomness r = wrap $ Randomness r return
 

loadImgFromFile :: MonadFree Game m => FilePath -> m (Img RGBA)
loadImgFromFile path = embedIO $ either error id `liftM` readImage path

cropImg :: ImageData -> (Int, Int) -> (Int, Int) -> ImageData
cropImg img (w, h) (x, y) = (runIdentity . computeP . extract (Z :. y :. x :. 0) (Z :. h :. w :. 4)) `onImg` img