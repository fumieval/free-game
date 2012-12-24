{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
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
) where

import Control.Monad.Free
import Control.Monad.Trans.Free (FreeT)
import Control.Monad
import Graphics.FreeGame.Bitmap
import Graphics.FreeGame.Input
import Graphics.FreeGame.Sound
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

instance Functor GameAction where
    fmap f (DrawPicture a cont) = DrawPicture a (f cont)
    fmap f (LoadPicture a cont) = LoadPicture a (f . cont)
    fmap f (PlaySound a cont)   = PlaySound a (f cont)
    fmap f (LoadSound a cont)   = LoadSound a (f . cont)
    fmap f (AskInput a cont)    = AskInput a (f . cont)
    fmap f (EmbedIO m) = EmbedIO (fmap f m)
    fmap f (Bracket m) = Bracket (fmap f m)
    fmap f (Tick cont) = Tick (f cont)

-- | Finalize the current frame and refresh the screen.
tick :: MonadFree GameAction m => m ()
tick = wrap $ Tick (return ())

-- | Embed arbitrary 'IO' actions into a 'Game' monad.
embedIO :: MonadFree GameAction m => IO a -> m a
embedIO m = wrap $ EmbedIO $ liftM return m

-- | Run a Game monad in the Game monad. resources (pictures, sounds) will be released when inner computation is done.
bracket :: MonadFree GameAction m => Game a -> m a
bracket m = wrap $ Bracket $ liftM return m

-- | Draw a 'Picture'.
drawPicture :: MonadFree GameAction m => Picture -> m ()
drawPicture pic = wrap $ DrawPicture pic (return ())

-- | Create 'Picture' from 'Bitmap'.
loadPicture :: MonadFree GameAction m => Bitmap -> m Picture
loadPicture img = wrap $ LoadPicture img return

-- | Play 'Sound'.
playSound :: MonadFree GameAction m => Sound -> m ()
playSound sound = wrap $ PlaySound sound (return ())

-- | Create 'Sound' from file.
loadSound :: MonadFree GameAction m => FilePath -> m Sound
loadSound path = wrap $ LoadSound path return

-- | Is the specified key is pressed?
askInput :: MonadFree GameAction m => Key -> m Bool
askInput key = wrap $ AskInput key return

-- | Get the mouse's state.
getMouseState :: MonadFree GameAction m => m MouseState
getMouseState = wrap $ GetMouseState return

-- | Apply the function to all pictures in 'DrawPicture'.
transPicture :: (Picture -> Picture) -> GameAction cont -> GameAction cont
transPicture f (DrawPicture p cont) = DrawPicture (f p) cont
transPicture _ x = x

-- | A 2D Picture.
data Picture
    -- | An abstract image object.
    = Image Unique
    -- | Combined picture from some pictures.
    | Pictures [Picture]
    -- | Rotated picture counterclockwise by the given angle (in radians).
    | Rotate Double Picture
    -- | Scaled picture.
    | Scale (Double, Double) Picture
    -- | A picture moved by the given coordinate.
    | Translate (Double, Double) Picture


-- | Parameters of the application.
data GameParam = GameParam {
        framePerSecond :: Int
        ,windowSize :: (Int, Int)
        ,windowTitle :: String
        ,windowed :: Bool
    }

defaultGameParam :: GameParam
defaultGameParam = GameParam 60 (640,480) "free-game" True