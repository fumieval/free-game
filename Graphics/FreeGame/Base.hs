{-# LANGUAGE ExistentialQuantification, FlexibleContexts, FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.FreeGame.Base
-- Copyright   :  (C) 2012 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinsohita <fumiexcel@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
-- Abstract structure that represents user interfaces
----------------------------------------------------------------------------

module Graphics.FreeGame.Base (
    Game
    ,GameAction(..)
    ,GameParam(..)
    ,Picture(..)
    ,transPicture
    ,defaultGameParam
    ,tick
    ,playSound
    ,drawPicture
    ,loadPicture
    ,askInput
    ,loadSound
    ,embedIO
    ,bracket
    ,getRealTime
    ,resetRealTime
    ,randomness
) where

import Control.Monad.Free
import Control.Monad.Trans.Free (FreeT)
import Control.Monad
import Graphics.FreeGame.Bitmap
import Graphics.FreeGame.Input
import Graphics.FreeGame.Sound
import System.Random
import qualified Data.ByteString.Lazy as BL
import Data.Unique

type Game = Free GameAction

data GameAction cont
    = Tick cont
    | EmbedIO (IO cont)
    | Bracket (Game cont)

    | DrawPicture Picture cont
    | LoadPicture Bitmap (Picture -> cont) 

    | PlaySound Sound cont
    | LoadSound FilePath (Sound -> cont)

    | AskInput Key (Bool -> cont)
    | GetMouseState (MouseState -> cont)

    | GetRealTime (Float -> cont)
    | ResetRealTime cont

    | forall a. Random a => Randomness (a, a) (a -> cont)

instance Functor GameAction where
    fmap f (DrawPicture a cont) = DrawPicture a (f cont)
    fmap f (LoadPicture a cont) = LoadPicture a (f . cont)
    fmap f (PlaySound a cont)   = PlaySound a (f cont)
    fmap f (LoadSound a cont)   = LoadSound a (f . cont)
    fmap f (AskInput a cont)    = AskInput a (f . cont)
    fmap f (Randomness a cont)  = Randomness a (f . cont)
    fmap f (EmbedIO m) = EmbedIO (fmap f m)
    fmap f (Bracket m) = Bracket (fmap f m)
    fmap f (GetRealTime cont) = GetRealTime (f . cont)
    fmap f (ResetRealTime cont) = ResetRealTime (f cont)
    fmap f (Tick cont) = Tick (f cont)

-- | finalizes the current frame and refresh the screen.
tick :: MonadFree GameAction m => m ()
tick = wrap $ Tick (return ())

-- | embeds arbitary 'IO' actions into a Game monad.
embedIO :: MonadFree GameAction m => IO a -> m a
embedIO m = wrap $ EmbedIO $ liftM return m

-- | runs a Game monad in the Game monad. resources (e.g.pictures, sounds) will be released automatically.
bracket :: MonadFree GameAction m => Game a -> m a
bracket m = wrap $ Bracket $ liftM return m

-- | draws a 'Picture'.
drawPicture :: MonadFree GameAction m => Picture -> m ()
drawPicture pic = wrap $ DrawPicture pic (return ())

-- | creates 'Picture' from 'Bitmap'.
loadPicture :: MonadFree GameAction m => Bitmap -> m Picture
loadPicture img = wrap $ LoadPicture img return

-- | plays 'Sound'.
playSound :: MonadFree GameAction m => Sound -> m ()
playSound sound = wrap $ PlaySound sound (return ())

-- | creates 'Sound' from file.
loadSound :: MonadFree GameAction m => FilePath -> m Sound
loadSound path = wrap $ LoadSound path return

-- | gets specified key's state.
askInput :: MonadFree GameAction m => Key -> m Bool
askInput key = wrap $ AskInput key return

-- | gets elapsed time since program began or 'resetRealTime' was called.
getRealTime :: MonadFree GameAction m => m Float
getRealTime = wrap $ GetRealTime return

-- | resets the elapsed time.
resetRealTime :: MonadFree GameAction m => m ()
resetRealTime = wrap $ ResetRealTime (return ())

-- | gets random value from specified range.
randomness :: (Random r, MonadFree GameAction m) => (r, r) -> m r
randomness r = wrap $ Randomness r return

-- | applies the function to all pictures in 'DrawPicture'.
transPicture :: (Picture -> Picture) -> GameAction cont -> GameAction cont
transPicture f (DrawPicture p cont) = DrawPicture (f p) cont
transPicture _ x = x

-- | A 2D Picture
data Picture
    = Image Unique -- | Abstract image object.
    | Transform Picture -- | allow image transforming of Rotate/Scale.
    | NoTransform Picture -- | don't allow image transforming of Rotate/Scale.
    | Pictures [Picture] -- | Combined picture from some pictures.
    | Rotate Double Picture -- | Rotated picture counterclockwise by the given angle (in radians).
    | Scale Double Picture -- | Scaled picture.
    | Translate (Double, Double) Picture -- | A picture moved by the given coordinate.

-- | Parameters of the application.
data GameParam = GameParam {
        framePerSecond :: Int
        ,windowSize :: (Int, Int)
        ,windowTitle :: String
        ,windowed :: Bool
        ,randomSeed :: Maybe Int
    }

defaultGameParam = GameParam 60 (640,480) "free-game" True Nothing