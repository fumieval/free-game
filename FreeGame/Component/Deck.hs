{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
module FreeGame.Component.Deck (emptyDeck, Source(..), Actions(..)) where
import FreeGame.Component
import Control.Lens
import Linear
import FreeGame.Types
import Control.Monad.State.Strict
import FreeGame.Data.Wave

data Actions x where
  Load :: Source (V2 Float) -> Actions ()
  Unload :: Actions ()
  Play :: Actions ()
  Stop :: Actions ()
  RMS :: Int -> Actions (V2 Double)
  Pull :: Time -> Int -> Actions [V2 Float]

data States = States
  { _src :: Maybe (Source (V2 Float))
  , _pos :: Time
  , _playing :: Bool
  , _sampleRate :: Double }

instance Audio Actions where
  pullAudio = Pull

--
src :: Lens' States (Maybe (Source (V2 Float)))
src f s = f (_src s) <&> \a -> s { _src = a }
pos :: Lens' States Time
pos f s = f (_pos s) <&> \a -> s { _pos = a }
playing :: Lens' States Bool
playing f s = f (_playing s) <&> \a -> s { _playing = a }
sampleRate :: Lens' States Double
sampleRate f s = f (_sampleRate s) <&> \a -> s { _sampleRate = a }

emptyDeck :: Monad m => Component Actions m
emptyDeck = go $ States Nothing 0 False 44100 where -- FIXME: sample rate
  -- go :: States -> Actions a -> (a, States)
  go s = Component $ \e -> liftM (fmap go) $ runStateT (handle e) s

handle :: MonadState States m => Actions a -> m a
handle (Load s) = src ?= s
handle Unload = src .= Nothing
handle Play = playing .= True
handle Stop = playing .= False
handle (RMS n) = use src >>= \case
  Just (Source s) -> do
    r <- use sampleRate
    t <- use pos
    let t0 = t - fromIntegral (n-1)/r
    return $ fmap realToFrac $ fmap (/fromIntegral n) $ sum $ map (fmap (^(2::Int)) . s) [t0, t0 + 1 / r..t]
  Nothing -> return 0

handle (Pull dt n) = use src >>= \case
  Just (Source s) -> do
    pl <- use playing
    t0 <- use pos
    if pl
      then do
        r <- use sampleRate
        pos += dt
        return [s t | t <- [t0,t0 + dt / fromIntegral n..t0 + dt - 1 / r]]
      else do
        return $ replicate n zero
  Nothing -> return $ replicate n zero