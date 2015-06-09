{-# LANGUAGE Rank2Types #-}
module FreeGame.Internal.Finalizer (FinalizerT(..), finalizer, runFinalizerT, execFinalizerT, mapFinalizerT) where

import Control.Monad.IO.Class
import Control.Monad.Trans
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

-- | An action with explicit releasing action.
newtype FinalizerT m a = FinalizerT
    { unFinalizerT :: forall r. (a -> m r) -> (IO () -> r -> m r) -> m r }

-- | Add a finalizer.
finalizer :: Monad m => IO () -> FinalizerT m ()
finalizer m = FinalizerT $ \p f -> p () >>= f m

instance Functor (FinalizerT m) where
    fmap f (FinalizerT g) = FinalizerT $ \p -> g (p . f)

instance Applicative (FinalizerT m) where
    pure a = FinalizerT $ \p _ -> p a
    FinalizerT ff <*> FinalizerT fa = FinalizerT $ \p f -> ff (\a -> fa (\b -> p (a b)) f) f

instance Monad (FinalizerT m) where
    return a = FinalizerT $ \p _ -> p a
    FinalizerT rf >>= k = FinalizerT $ \p f -> rf (\x -> unFinalizerT (k x) p f) f

instance MonadIO m => MonadIO (FinalizerT m) where
    liftIO m = FinalizerT $ \r _ -> liftIO m >>= r
    {-# INLINE liftIO #-}

instance MonadTrans FinalizerT where
    lift m = FinalizerT $ \r _ -> m >>= r
    {-# INLINE lift #-}

-- | Run the action and run all associated finalizers.
runFinalizerT :: Monad m => FinalizerT m a -> m (a, IO ())
runFinalizerT (FinalizerT z) = z (\a -> return (a, return ())) (\m (r, fs) -> return (r, m >> fs))

execFinalizerT :: MonadIO m => FinalizerT m a -> m a
execFinalizerT m = do
    (a, fin) <- runFinalizerT m
    liftIO fin
    return a

mapFinalizerT :: (Monad m, Monad n) => (forall x. m x -> n x) -> FinalizerT m a -> FinalizerT n a
mapFinalizerT t m = FinalizerT $ \p f -> do
    (a, fin) <- t (runFinalizerT m)
    r <- p a
    f fin r