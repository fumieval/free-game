module Graphics.FreeGame.Control.Vein where

import qualified Control.Category
import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Monoid

newtype Vein m i o = Vein { runVein :: i -> m (o, Vein m i o) }

execVein :: Monad m => Vein m i o -> i -> m (Vein m i o)
execVein v i = liftM snd $ runVein v i

instance Monad m => Control.Category.Category (Vein m) where
    id = Vein $ \x -> return (x, Control.Category.id)
    Vein f . Vein g = Vein $ \x -> do
        (y, b) <- g x
        (z, a) <- f y
        return (z, a Control.Category.. b)

instance Monad m => Arrow (Vein m) where
    arr f = Vein $ \x -> return (f x, arr f)
    first (Vein f) = Vein $ \(x, y) -> do
        (x', a) <- f x
        return ((x', y), first a)
    second (Vein f) = Vein $ \(y, x) -> do
        (x', a) <- f x
        return ((y, x'), second a)

instance Monad m => Functor (Vein m i) where
    fmap f r = r >>> arr f

instance Monad m => Applicative (Vein m i) where
    pure x = Vein $ \_ -> return (x, pure x)
    Vein f <*> Vein g = Vein $ \x -> do
        (h, r) <- f x
        (v, s) <- g x
        return (h v, r <*> s)

instance (Monad m, Monoid o) => Monoid (Vein m i o) where
    mempty = pure mempty
    mappend a b = mappend <$> a <*> b

actionAfter :: Monad m => m () -> Vein m i o -> Vein m i o
actionAfter m v = Vein $ \i -> do
    (o, cont) <- runVein v i
    m
    return (o, actionAfter m cont)

actionBefore :: Monad m => m () -> Vein m i o -> Vein m i o
actionBefore m v = Vein $ \i -> do
    m
    (o, cont) <- runVein v i
    return (o, actionBefore m cont)

feedbackVein :: Monad m => a -> Vein m a a -> m ()
feedbackVein a v = runVein v a >>= uncurry feedbackVein

foreverVein :: Monad m => Vein m () a -> m ()
foreverVein v = execVein v () >>= foreverVein

wrapAction :: Monad m => (i -> m o) -> Vein m i o
wrapAction f = Vein $ \i -> flip (,) (wrapAction f) `liftM` f i

wrapStateful :: Monad m => (i -> StateT s m o) -> s -> Vein m i o
wrapStateful m s = Vein $ \i -> second (wrapStateful m) `liftM` (m i `runStateT` s)

bundle :: Monad m => [Vein m i o] -> Vein m i [o]
bundle vs = Vein $ \i -> liftM (second bundle . unzip) $ forM vs (flip runVein i)

bundleFinite :: Monad m => [Vein (MaybeT m) i o] -> Vein m i [o]
bundleFinite vs = Vein $ \i -> liftM (second bundleFinite . unzip . catMaybes)
    $ forM vs (runMaybeT . flip runVein i)

