{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DeriveFunctor #-}
module FreeGame.Text (TextF(..), TextT, runTextT, runTextT_, text) where

import Control.Lens
import Data.String
import Data.BoundingBox
import Data.Hashable
import Graphics.Holz.Font
import FreeGame.Class
import FreeGame.Data.Bitmap
import FreeGame.Instances ()
import Control.Monad.Trans.Free
import Control.Monad.State
import Linear
import Codec.Picture

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
            (bmp@(Image w h _), offset, adv) <- fromFinalizer $ renderChar font (realToFrac siz) ch
            pen <- get
            let offset' = fmap realToFrac offset + V2 (fromIntegral w / 2) (fromIntegral h / 2)
            translate (pen + offset') $ bitmap $ Bitmap bmp $ hash (siz, ch)
            let pen' = pen + fmap realToFrac adv
            put $ if cond pen'
                then pen'
                else V2 x0 (view _y pen + advV)
            go cont
    advV = siz * realToFrac (metricsAscent font - metricsDescent font) * 1.1
    (V2 x0 y0, cond) = maybe (zero, const True) (\b -> (b ^. position zero, flip isInside b)) bbox

runTextT_ :: (FromFinalizer m, Monad m, Picture2D m) => Maybe (Box V2 Double) -> Font -> Double -> TextT m () -> m ()
runTextT_ = runTextT
{-# INLINE runTextT_ #-}

-- | Render a 'String'.
text :: (FromFinalizer m, Monad m, Picture2D m) => Font -> Double -> String -> m ()
text font siz str = runTextT Nothing font siz (fromString str)
