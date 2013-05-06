{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DeriveFunctor #-}
module Graphics.UI.FreeGame.Text (TextF(..), TextT, runTextT, text) where

import Data.String
import Graphics.UI.FreeGame.Base
import Graphics.UI.FreeGame.Types
import Graphics.UI.FreeGame.Internal.Raindrop
import Graphics.UI.FreeGame.Data.Font
import Graphics.UI.FreeGame.Data.Bitmap
import Control.Monad.Trans.Free
import Control.Monad.State
import Linear

data TextF a = TypeChar Char a deriving Functor

type TextT = FreeT TextF

instance Monad m => IsString (TextT m ()) where
    fromString str = mapM_ (\c -> liftF (TypeChar c ())) str

runTextT :: (FromFinalizer m, Monad m, Picture2D m) => Maybe (BoundingBox Float) -> Font -> Float -> TextT m a -> m a
runTextT bbox font size = flip evalStateT (V2 x0 y0) . go where
    go m = lift (runFreeT m) >>= \r -> case r of
        Pure a -> return a
        Free (TypeChar '\n' cont) -> do
            modify $ over _x (const x0) . over _y (+advV)
            go cont
        Free (TypeChar ch cont) -> do
            RenderedChar bmp (V2 x y) adv <- fromFinalizer $ charToBitmap font size ch
            pen <- get
            let (w,h) = bitmapSize bmp
                offset = pen ^+^ V2 (x + fromIntegral w / 2) (y + fromIntegral h / 2)
            translate offset $ fromBitmap bmp
            let pen' = over _x (+adv) pen
            put $ if cond pen'
                then pen'
                else V2 x0 (view _y pen + advV)
            go cont
    advV = size * (metricsAscent font - metricsDescent font) * 1.1
    (V2 x0 y0, cond) = maybe (zero, const True) (\b -> (view _TopLeft b, flip inBoundingBox b)) bbox

text :: (FromFinalizer m, Monad m, Picture2D m) => Font -> Float -> String -> m ()
text font size str = runTextT Nothing font size (fromString str)