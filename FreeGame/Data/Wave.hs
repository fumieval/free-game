module FreeGame.Data.Wave (
    Wave(..)
    , _WaveData
    , _WaveHash
    , toWave
    , makeWave
    , loadWaveFromFile
    , Voice(..)
) where

import Data.Hashable
import System.Random
import Data.WAVE
import Linear
import Control.Monad.IO.Class

instance Hashable a => Hashable (V2 a) where
    hashWithSalt s (V2 a b) = s `hashWithSalt` a `hashWithSalt` b

data Wave = WaveData [V2 Float] Int

-- | @'_WaveData' :: Lens' 'Wave' [V2 Float]@
_WaveData :: Functor f => ([V2 Float] -> f [V2 Float]) -> Wave -> f Wave
_WaveData f (WaveData a h) = fmap (\a' -> WaveData a' h) (f a)

-- | @'_WaveHash' :: Lens' 'Wave' ('Int')@
_WaveHash :: Functor f => (Int -> f Int) -> Wave -> f Wave
_WaveHash f (WaveData a h) = fmap (\h' -> WaveData a h') (f h)

toWave :: [V2 Float] -> Wave
toWave w = WaveData w (hash w)

makeWave :: [V2 Float] -> IO Wave
makeWave w = fmap (WaveData w) randomIO

loadWaveFromFile :: MonadIO m => FilePath -> m Wave
loadWaveFromFile path = liftIO $ do
    WAVE _ ss <- getWAVEFile path
    makeWave [V2 (f l) (f r) | [l, r] <- ss]
    where

        maxb = fromIntegral (maxBound :: WAVESample)
        minb = fromIntegral (minBound :: WAVESample)
        f v
            | v >= 0 = fromIntegral v / maxb
            | otherwise = -(fromIntegral v) / minb

newtype Voice = Voice Int