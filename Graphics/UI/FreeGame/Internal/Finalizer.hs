{-# LANGUAGE Rank2Types #-}
module Graphics.UI.FreeGame.Internal.Finalizer (FinalizerT(..), finalizer, runFinalizerT) where

import Control.Monad.IO.Class
import Control.Applicative

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

-- | Run the action and run all associated finalizers.
runFinalizerT :: MonadIO m => FinalizerT m a -> m a
runFinalizerT (FinalizerT z) = do
    (fin, a) <- z (\a -> return (return (), a)) (\m (fs, r) -> return (m >> fs,　r))
    liftIO fin
    return a