module Graphics.UI.FreeGame.Data.Wave (
    Wave(..)
    , _WaveData
    , _WaveHash
    , toWave
    , makeWave
) where

import Data.Hashable
import System.Random

data Wave = WaveData [(Float, Float)] Int

-- | @'_WaveData' :: Lens' 'Wave' [(Float, Float)]@
_WaveData :: Functor f => ([(Float, Float)] -> f [(Float, Float)]) -> Wave -> f Wave
_WaveData f (WaveData a h) = fmap (\a' -> WaveData a' h) (f a)

-- | @'_WaveHash' :: Lens' 'Wave' ('Int')@
_WaveHash :: Functor f => (Int -> f Int) -> Wave -> f Wave
_WaveHash f (WaveData a h) = fmap (\h' -> WaveData a h') (f h)

toWave :: [(Float, Float)] -> Wave
toWave w = WaveData w (hash w)

makeWave :: [(Float, Float)] -> IO Wave
makeWave w = fmap (WaveData w) randomIO