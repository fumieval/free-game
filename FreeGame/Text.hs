{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DeriveFunctor #-}
module FreeGame.Text (TextF(..), TextT, runTextT, runTextT_, text) where

import Data.String
import FreeGame.Types
import FreeGame.Internal.Raindrop
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
runTextT :: (FromFinalizer m, Monad m, Picture2D m) => Maybe (BoundingBox Double) -> Font -> Double -> TextT m a -> m a
runTextT bbox font size = flip evalStateT (V2 x0 y0) . go where
    go m = lift (runFreeT m) >>= \r -> case r of
        Pure a -> return a
        Free (TypeChar '\n' cont) -> do
            modify $ over _x (const x0) . over _y (+advV)
            go cont
        Free (TypeChar ch cont) -> do
            RenderedChar bmp offset adv <- fromFinalizer $ charToBitmap font size ch
            pen <- get
            translate (pen + offset) $ bitmap bmp
            let pen' = over _x (+adv) pen
            put $ if cond pen'
                then pen'
                else V2 x0 (view _y pen + advV)
            go cont
    advV = size * (metricsAscent font - metricsDescent font) * 1.1
    (V2 x0 y0, cond) = maybe (zero, const True) (\b -> (view _TopLeft b, flip inBoundingBox b)) bbox

runTextT_ :: (FromFinalizer m, Monad m, Picture2D m) => Maybe (BoundingBox Double) -> Font -> Double -> TextT m () -> m ()
runTextT_ = runTextT
{-# INLINE runTextT_ #-}

-- | Render a 'String'.
text :: (FromFinalizer m, Monad m, Picture2D m) => Font -> Double -> String -> m ()
text font size str = runTextT Nothing font size (fromString str)
