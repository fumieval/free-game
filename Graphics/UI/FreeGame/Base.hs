{-# LANGUAGE Rank2Types, DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
import Control.Monad.Free.Class
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader

newtype UI f a = UI { unUI :: forall r. (r -> r) -> (f r -> r) -> (a -> r) -> r } deriving Functor

instance Monad (UI f) where
    return a = UI $ \_ _ p -> p a
    m >>= f = UI $ \ct cw cp -> unUI m ct cw (\a -> unUI (f a) ct cw cp)

instance Functor f => MonadFree f (UI f) where
    wrap fm = UI $ \ct cw cp -> cw $ fmap (\(UI m) -> m ct cw cp) fm

class MonadTick m where
    tick :: m ()

instance MonadTick (UI f) where
    tick = UI $ \ct _ cp -> ct (cp ())

instance Functor f => MonadFree f (UI' f) where
    wrap = WrapUI

stepUI :: MonadFree f m => UI' f a -> m (Either (UI' f a) a)
stepUI (WrapUI fm) = wrap (fmap step fm)
stepUI (PureUI a) = return (Right a)
stepUI (TickUI m) = return (Left m)

data UI' f a = WrapUI (f (UI' f a)) | PureUI a | TickUI (UI' f a) deriving Functor

instance Functor f => Monad (UI' f) where
    return = PureUI
    WrapUI fm >>= f = WrapUI $ fmap (>>=f) fm
    PureUI a >>= f = f a
    TickUI m >>= f = TickUI (m >>= f)

cloneUI :: (MonadTick m, MonadFree f m) => UI f a -> m a
cloneUI m = unUI m (tick>>) wrap return

fuseUI :: Functor f => UI' f a -> UI f a
fuseUI m = UI $ \kt kf kp -> go kt kf kp m where
    go kt kf kp (PureUI a) = kp a
    go kt kf kp (WrapUI fm) = kf (fmap (go kt kf kp) fm)
    go kt kf kp (TickUI m) = kt (go kt kf kp m)
