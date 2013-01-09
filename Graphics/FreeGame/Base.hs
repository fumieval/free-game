{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.FreeGame.Base
-- Copyright   :  (C) 2012 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinsohita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Abstract structures that represents user interfaces
----------------------------------------------------------------------------

module Graphics.FreeGame.Base (
    -- * Types
    Game
    ,GameAction(..)

    -- * Basic operations
    ,tick
    ,embedIO
    ,bracket
    ,quitGame

    -- * Pictures
    ,Vec2(..)
    ,Picture(..)
    ,transPicture
    ,drawPicture
    
    -- * Inputs
    ,askInput
    ,getMouseState

    -- * Settings
    ,GameParam(..)
    ,defaultGameParam

    -- * Deprecated
    ,loadPicture
) where

import Control.Monad.Free
import Control.Monad
import Graphics.FreeGame.Data.Color
import Graphics.FreeGame.Data.Bitmap
import Graphics.FreeGame.Input
import Data.Vect

infixr 5 `Translate`
infixr 5 `Rotate`
infixr 5 `Scale`
infixr 5 `Colored`

-- | 'Game' is a 'Monad' that abstracts user interfaces.
type Game = Free GameAction

-- | A base for 'Game' monad.
data GameAction a
    = Tick a
    | EmbedIO (IO a)
    | Bracket (Game a)
    | DrawPicture Picture a
    | AskInput Key (Bool -> a)
    | GetMouseState (MouseState -> a)
    | QuitGame

instance Functor GameAction where
    fmap f (DrawPicture a cont) = DrawPicture a (f cont)
    fmap f (AskInput a cont)    = AskInput a (f . cont)
    fmap f (GetMouseState cont) = GetMouseState (f . cont)
    fmap f (EmbedIO m) = EmbedIO (fmap f m)
    fmap f (Bracket m) = Bracket (fmap f m)
    fmap f (Tick cont) = Tick (f cont)
    fmap _ QuitGame = QuitGame

-- | Finalize the current frame and refresh the screen.
tick :: MonadFree GameAction m => m ()
tick = wrap $ Tick (return ())

-- | Embed arbitrary 'IO' actions into a 'Game' monad.
embedIO :: MonadFree GameAction m => IO a -> m a
embedIO m = wrap $ EmbedIO $ liftM return m

-- | Run a Game monad in a Game monad. resources (e.g. pictures) will be released when inner computation is done.
bracket :: MonadFree GameAction m => Game a -> m a
bracket m = wrap $ Bracket $ liftM return m

-- | Break the current computation.
quitGame :: MonadFree GameAction m => m a
quitGame = wrap QuitGame

-- | Draw a 'Picture'.
drawPicture :: MonadFree GameAction m => Picture -> m ()
drawPicture pic = wrap $ DrawPicture pic (return ())

-- | Is the specified 'Key' is pressed?
askInput :: MonadFree GameAction m => Key -> m Bool
askInput key = wrap $ AskInput key return

-- | Get the mouse's state.
getMouseState :: MonadFree GameAction m => m MouseState
getMouseState = wrap $ GetMouseState return

-- | Lift a picture transformation into transformation of 'GameAction'
transPicture :: (Picture -> Picture) -> GameAction cont -> GameAction cont
transPicture f (DrawPicture p cont) = DrawPicture (f p) cont
transPicture _ x = x

-- | A 2D Picture.
data Picture
    -- | A 'Bitmap' as a 'Picture'.
    = BitmapPicture Bitmap
    -- | A picture consist of some 'Picture's.
    | Pictures [Picture]
    -- | A picture that may have side effects.
    | IOPicture (IO Picture)
    -- | Rotated picture by the given angle (in degrees, counterclockwise).
    | Rotate Float Picture
    -- | Scaled picture.
    | Scale Vec2 Picture
    -- | A picture translated by the given coordinate.
    | Translate Vec2 Picture
    -- | Colored picture.
    | Colored Color Picture

-- | Parameters of the application.
data GameParam = GameParam {
        framePerSecond :: Int
        ,windowSize :: (Int, Int)
        ,windowTitle :: String
        ,windowed :: Bool
    }

-- | 640*480(windowed), 60fps
defaultGameParam :: GameParam
defaultGameParam = GameParam 60 (640,480) "free-game" True

{-# DEPRECATED loadPicture "No longer needed; use BitmapPicture instead" #-}
-- | Create a 'Picture' from 'Bitmap'.
loadPicture :: MonadFree GameAction m => Bitmap -> m Picture
loadPicture = return . BitmapPicture
