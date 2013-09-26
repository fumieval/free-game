{-# LANGUAGE Rank2Types, DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
import Control.Monad.Free.Class
import Control.Monad

class MonadTick m where
    tick :: m ()

instance Functor f => MonadFree f (UI f) where
    wrap = WrapUI

data UI f a = WrapUI (f (UI f a)) | PureUI a | TickUI (UI f a) deriving Functor

instance Functor f => Monad (UI f) where
    return = PureUI
    WrapUI fm >>= f = WrapUI $ fmap (>>=f) fm
    PureUI a >>= f = f a
    TickUI m >>= f = TickUI (m >>= f)

stepUI :: MonadFree f m => UI f a -> m (Either (UI f a) a)
stepUI (WrapUI fm) = wrap (fmap step fm)
stepUI (PureUI a) = return (Right a)
stepUI (TickUI m) = return (Left m)

cloneUI :: (MonadTick m, MonadFree f m) => UI f a -> m a
cloneUI (WrapUI fm) = wrap (fmap cloneUI fm)
cloneUI (PureUI a) = return a
cloneUI (TickUI m) = tick >> cloneUI m
