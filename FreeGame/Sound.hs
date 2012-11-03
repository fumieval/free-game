module FreeGame.Sound where

import Data.Unique

newtype WaveData = WaveData Unique deriving (Eq, Ord)

data Sound = Wave WaveData
