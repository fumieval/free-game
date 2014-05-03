{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DeriveFunctor #-}
module FreeGame.Text (TextF(..), TextT, runTextT, runTextT_, text) where

import Control.Lens
import Data.String
import Data.BoundingBox
import FreeGame.Data.Font
import FreeGame.Class
import FreeGame.Instances ()
import Control.Monad.Trans.Free
import Control.Monad.State
import Linear

data TextF a = TypeChar Char a deriving Functor

type TextT = FreeT TextF

instance Monad m => IsString (TextT m ()) where
    fromString str = mapM_ (\c -> liftF (TypeChar c ())) str

-- | Render a 'TextT'.
runTextT :: (FromFinalizer m, Monad m, Picture2D m) => Maybe (Box V2 Double) -> Font -> Double -> TextT m a -> m a
runTextT bbox font siz = flip evalStateT (V2 x0 y0) . go where
    go m = lift (runFreeT m) >>= \r -> case r of
        Pure a -> return a
        Free (TypeChar '\n' cont) -> do
            _x .= x0
            _y += advV
            go cont
        Free (TypeChar ch cont) -> do
            RenderedChar bmp offset adv <- fromFinalizer $ charToBitmap font siz ch
            pen <- get
            translate (pen + offset) $ bitmap bmp
            let pen' = over _x (+adv) pen
            put $ if cond pen'
                then pen'
                else V2 x0 (view _y pen + advV)
            go cont
    advV = siz * (metricsAscent font - metricsDescent font) * 1.1
    (V2 x0 y0, cond) = maybe (zero, const True) (\b -> (b ^. position zero, flip isInside b)) bbox

runTextT_ :: (FromFinalizer m, Monad m, Picture2D m) => Maybe (Box V2 Double) -> Font -> Double -> TextT m () -> m ()
runTextT_ = runTextT
{-# INLINE runTextT_ #-}

-- | Render a 'String'.
text :: (FromFinalizer m, Monad m, Picture2D m) => Font -> Double -> String -> m ()
text font siz str = runTextT Nothing font siz (fromString str)