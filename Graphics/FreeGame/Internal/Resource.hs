{-# LANGUAGE DeriveFunctor, FlexibleContexts #-}
module Graphics.FreeGame.Internal.Resource (ResourceT, finalizer, runResourceT) where

import Control.Monad.Trans.Free
import Control.Monad.IO.Class

data ResourceF a = Finalizer (IO ()) a deriving Functor

finalizer :: MonadFree ResourceF m => IO () -> m ()
finalizer m = wrap $ Finalizer m (return ())

runResourceT :: MonadIO m => ResourceT m a -> m a
runResourceT = run (return ()) where
    run f m = runFreeT m >>= \r -> case r of
        Free (Finalizer f' cont) -> run (f' >> f) cont
        Pure a -> do
            liftIO f
            return a

type ResourceT = FreeT ResourceF