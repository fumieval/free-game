{-# LANGUAGE FlexibleContexts, DeriveFunctor, FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.FreeGame.Base
-- Copyright   :  (C) 2012-2013 Fumiaki Kinoshita
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
    ,Color(..)
    ,Picture(..)
    ,transPicture
    ,drawPicture
    
    -- * Inputs
    ,getButtonState
    ,getMousePosition
    ,getMouseWheel

    -- * Settings
    ,GameParam(..)
    ,defaultGameParam
    ,getCurrentGameParam

    -- * Deprecated
    ,askInput
    ,getMouseState
) where

import Control.Monad.Free
import Control.Monad.Free.Church
import Control.Monad
import Graphics.FreeGame.Data.Color
import Graphics.FreeGame.Data.Bitmap
import Graphics.FreeGame.Input
import Graphics.FreeGame.Internal.Resource
import Control.Monad.IO.Class
import Data.Vect
import Data.Void

infixr 5 `Translate`
infixr 5 `Rotate`
infixr 5 `Scale`
infixr 5 `Colored`

-- | 'Game' is a 'Monad' that abstracts user interfaces.
type Game = Free GameAction

instance MonadIO Game where
    liftIO = embedIO

-- | A base for 'Game' monad.
data GameAction a
    = Tick a
    | EmbedIO (IO a)
    | Bracket (Game a)
    | DrawPicture Picture a
    | GetButtonState Button (Bool -> a)
    | GetMousePosition (Vec2 -> a)
    | GetMouseWheel (Int -> a)
    | GetGameParam (GameParam -> a)
    | QuitGame
    deriving Functor

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
quitGame :: MonadFree GameAction m => m Void
quitGame = wrap QuitGame

-- | Draw a 'Picture'.
drawPicture :: MonadFree GameAction m => Picture -> m ()
drawPicture pic = wrap $ DrawPicture pic (return ())

-- | Is the specified 'Button' is pressed?
getButtonState :: MonadFree GameAction m => Button -> m Bool
getButtonState key = wrap $ GetButtonState key return

getMouseWheel :: MonadFree GameAction m => m Int
getMouseWheel = wrap $ GetMouseWheel return

getMousePosition :: MonadFree GameAction m => m Vec2
getMousePosition = wrap $ GetMousePosition return

-- | Get the game params that apply to the currently running game.
getCurrentGameParam :: MonadFree GameAction m => m GameParam
getCurrentGameParam = wrap $ GetGameParam return

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
    -- | A picture that may have side effects(for internal use).
    | ResourcePicture (ResourceT IO Picture)
    -- | Rotated picture by the given angle (in degrees, counterclockwise).
    | Rotate Float Picture
    -- | Scaled picture.
    | Scale Vec2 Picture
    -- | A picture translated by the given coordinate.
    | Translate Vec2 Picture
    -- | Colored picture.
    | Colored Color Picture

-- | Parameters of the application.
data GameParam = GameParam
    {    framePerSecond :: Int
        ,windowSize :: (Int, Int)
        ,windowTitle :: String
        ,windowed :: Bool
        ,cursorVisible :: Bool
        ,clearColor :: Color
        ,windowOrigin :: Vec2
    } deriving Show

-- | 640*480(windowed), 60fps
defaultGameParam :: GameParam
defaultGameParam = GameParam 60 (640,480) "free-game" True True white (Vec2 0 0)

-- | Deprecated synonym for 'getMouseState'.
getMouseState :: MonadFree GameAction m => m MouseState
getMouseState = MouseState
    `liftM` getMousePosition
    `ap` askInput MouseLeft
    `ap` askInput MouseMiddle
    `ap` askInput MouseRight
    `ap` getMouseWheel
{-# DEPRECATED getMouseState "use getMousePosition and getMouseWheel instead." #-}

-- | Deprecated synonym for 'getButtonState'.
askInput :: MonadFree GameAction m => Button -> m Bool
askInput = getButtonState
{-# DEPRECATED askInput "use getButtonState instead." #-}