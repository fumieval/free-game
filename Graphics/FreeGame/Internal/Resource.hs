{-# LANGUAGE Rank2Types #-}
module Graphics.FreeGame.Internal.Resource (ResourceT(..), finalizer, runResourceT) where

import Control.Monad.IO.Class
import Control.Applicative

newtype ResourceT m a = ResourceT
    { unResourceT :: forall r. (a -> m r) -> (IO () -> r -> m r) -> m r }

finalizer :: Monad m => IO () -> ResourceT m ()
finalizer m = ResourceT $ \p f -> p () >>= f m

instance Functor (ResourceT m) where
    fmap f (ResourceT g) = ResourceT $ \p -> g (p . f)

instance Applicative (ResourceT m) where
    pure a = ResourceT $ \p _ -> p a
    ResourceT ff <*> ResourceT fa = ResourceT $ \p f -> ff (\a -> fa (\b -> p (a b)) f) f

instance Monad (ResourceT m) where
    return a = ResourceT $ \p _ -> p a
    ResourceT rf >>= k = ResourceT $ \p f -> rf (\x -> unResourceT (k x) p f) f

instance MonadIO m => MonadIO (ResourceT m) where
    liftIO m = ResourceT $ \r _ -> liftIO m >>= r

runResourceT :: MonadIO m => ResourceT m a -> m a
runResourceT (ResourceT z) = do
    (fin, a) <- z (\a -> return (return (), a)) (\m (fs, r) -> return (m >> fs,　r))
    liftIO fin
    return a