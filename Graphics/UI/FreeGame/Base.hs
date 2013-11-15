{-# LANGUAGE Rank2Types, DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
import Control.Monad.Free.Class
import Control.Monad

class MonadTick m where
    tick :: m ()

newtype TickM f a = TickM { unUI :: forall r. (f r -> r) -> (r -> r) -> (a -> r) -> r } deriving Functor

instance Functor f => MonadFree f (ReifiedUI f) where
    wrap = WrapUI

data TickM f a = Wrap (f (TickFree f a)) | Tick (TickFree f a) | Pure a deriving Functor

instance Functor f => Monad (TickFree f) where
    return = PureUI
    WrapUI fm >>= f = WrapUI $ fmap (>>=f) fm
    PureUI a >>= f = f a
    TickUI m >>= f = TickUI (m >>= f)

stepFree :: MonadFree f m => TickFree f a -> m (Either (TickFree f a) a)
stepFree (WrapUI fm) = wrap (fmap step fm)
stepFree (PureUI a) = return (Right a)
stepFree (TickUI m) = return (Left m)

cloneFree :: (MonadTick m, MonadFree f m) => TickFree f a -> m a
cloneFree (WrapUI fm) = wrap (fmap cloneUI fm)
cloneFree (PureUI a) = return a
cloneFree (TickUI m) = tick >> cloneUI m
