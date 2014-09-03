{-# LANGUAGE Rank2Types, ExistentialQuantification, KindSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module FreeGame.Component where

import FreeGame.Data.Bitmap
import FreeGame.Types
import Control.Monad.Free.Church
import Linear
import FreeGame.Class
import Control.Applicative
import Control.Monad.IO.Class

newtype Control (e :: * -> *) = Control Int

newtype Picture a = Picture { runPicture :: forall m. (Applicative m, Monad m, Picture2D m, Local m) => m a }

data Sys a = forall e. Functor e => Command (Control e) (e a)
  | forall e. Invoke (Component e System) (Control e -> a)
  | forall e. Graphic e => ConnectGraphic (Control e) a
  | forall e. DisconnectGraphic (Control e) a
  | forall e. Audio e => ConnectAudio (Control e) a
  | forall e. DisconnectAudio (Control e) a
  | forall e. HandleKeyboard e => ConnectKeyboard (Control e) a
  | forall e. DisconnectKeyboard (Control e) a
  | forall e. HandleMouse e => ConnectMouse (Control e) a
  | forall e. DisconnectMouse (Control e) a
  | TakeScreenShot (Bitmap -> a)
  | LiftIO (IO a)
--    | SetWindowTitle String a
--    | SetBoundingBox ()
  | Wait Double a
  | Stand a

instance Functor Sys where
  fmap f (Command c e) = Command c (fmap f e)
  fmap f (Invoke c j) = Invoke c (fmap f j)
  fmap f (ConnectGraphic c a) = ConnectGraphic c (f a)
  fmap f (DisconnectGraphic c a) = DisconnectGraphic c (f a)
  fmap f (ConnectAudio c a) = ConnectAudio c (f a)
  fmap f (DisconnectAudio c a) = DisconnectAudio c (f a)
  fmap f (ConnectKeyboard c a) = ConnectKeyboard c (f a)
  fmap f (ConnectMouse c a) = ConnectMouse c (f a)
  fmap f (DisconnectKeyboard c a) = DisconnectKeyboard c (f a)
  fmap f (DisconnectMouse c a) = DisconnectMouse c (f a)

  fmap f (TakeScreenShot a) = TakeScreenShot (fmap f a)
  fmap f (Wait t a) = Wait t (f a)
  fmap f (Stand a) = Stand (f a)
  fmap f (LiftIO m) = LiftIO (fmap f m)


type System = F Sys

type Time = Double

newtype Component e m = Component { runComponent :: forall x. e x -> m (x, Component e m) }

class HandleMouse f where
  cursorEvent :: Vec2 -> f ()
  scrollEvent :: Vec2 -> f ()
  mouseButtonEvent :: Int -> Bool -> f ()

class HandleKeyboard f where
  keyEvent :: Key -> Bool -> f ()

class Graphic e where
  pullGraphic :: Time -> e (Picture ())

class Audio e where
  pullAudio :: Time -> Int -> e [V2 Float]

instance MonadIO System where
  liftIO m = wrap $ LiftIO $ fmap return m

(.-) :: (MonadFree Sys m, Functor e) => Control e -> e x -> m x
c .- e = wrap $ Command c (fmap return e)

invoke :: MonadFree Sys m => Component e System -> m (Control e)
invoke c = wrap $ Invoke c return

connectMouse :: (MonadFree Sys m, HandleMouse e) => Control e -> m ()
connectMouse c = wrap (ConnectMouse c (return ()))

disconnectMouse :: MonadFree Sys m => Control e -> m ()
disconnectMouse c = wrap (DisconnectMouse c (return ()))

connectKeyboard :: (MonadFree Sys m, HandleKeyboard e) => Control e -> m ()
connectKeyboard c = wrap (ConnectKeyboard c (return ()))

disconnectKeyboard :: MonadFree Sys m => Control e -> m ()
disconnectKeyboard c = wrap (DisconnectKeyboard c (return ()))

connectGraphic :: (MonadFree Sys m, Graphic e) => Control e -> m ()
connectGraphic c = wrap (ConnectGraphic c (return ()))

disconnectGraphic :: MonadFree Sys m => Control e -> m ()
disconnectGraphic c = wrap (DisconnectGraphic c (return ()))

connectAudio :: (MonadFree Sys m, Audio e) => Control e -> m ()
connectAudio c = wrap (ConnectAudio c (return ()))

disconnectAudio :: MonadFree Sys m => Control e -> m ()
disconnectAudio c = wrap (DisconnectAudio c (return ()))

stand :: MonadFree Sys m => m ()
stand = wrap $ Stand (return ())

wait :: MonadFree Sys m => Double -> m ()
wait t = wrap $ Wait t (return ())