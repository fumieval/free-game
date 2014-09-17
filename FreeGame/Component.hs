{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}

module FreeGame.Component where

import FreeGame.Types
import Linear
import FreeGame.Picture
import Control.Monad.IO.Class

newtype Control s (e :: * -> *) = Control Int

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

class MonadIO m => MonadSystem s m where
  type Base m :: * -> *
  (.-) :: Control s e -> e a -> m a
  invoke :: Component e (Base m) -> m (Control s e)
  connectMouse :: HandleMouse e => Control s e -> m ()
  connectKeyboard :: HandleKeyboard e => Control s e -> m ()
  connectGraphic :: Graphic e => Control s e -> m ()
  connectAudio :: Audio e => Control s e -> m ()
  disconnectMouse :: Control s e -> m ()
  disconnectKeyboard :: Control s e -> m ()
  disconnectGraphic :: Control s e -> m ()
  disconnectAudio :: Control s e -> m ()
  stand :: m ()
  wait :: Double -> m ()

oneshot :: (Functor e, Monad m) => (forall a. e (m a) -> m a) -> Component e m
oneshot m = go where
  go = Component $ \e -> m (fmap return e) >>= \a -> return (a, go)

data PullGraphic a = PullGraphic Time (Picture () -> a) deriving Functor

instance Graphic PullGraphic where
  pullGraphic t = PullGraphic t id
