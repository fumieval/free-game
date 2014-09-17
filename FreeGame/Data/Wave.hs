{-# LANGUAGE BangPatterns #-}
module FreeGame.Data.Wave (
  Source(..),
  readWAVE
) where

import Data.WAVE
import Linear
import FreeGame.Types
import Control.Monad.IO.Class
import qualified Data.Vector.Unboxed as V

newtype Source a = Source (Time -> a)

readWAVE :: MonadIO m => FilePath -> m (Source (V2 Float))
readWAVE path = liftIO $ do
  WAVE h ss <- getWAVEFile path
  
  return $ Source $ sample h $ V.fromList (map fr ss)
  where
    fr [!a, !b] = (realToFrac $ sampleToDouble a, realToFrac $ sampleToDouble b)
    fr _ = (0, 0)
    sample h v t = maybe zero (uncurry V2) $ v V.!? floor (t * fromIntegral (waveFrameRate h))

-- TODO: Lazy processing