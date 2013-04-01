{-# LANGUAGE FlexibleContexts, DeriveFunctor, FlexibleInstances, UndecidableInstances #-}
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

module Graphics.UI.FreeGame.Base (
    -- * Types
    ,UI(..)

    -- * Basic operations
    ,tick
    ,bracket
    ,quitGame
    -- * Pictures
    ,Vec2(..)
    ,Color(..)
    ,Picture(..)
    ,transPicture
    ,draw
    
    -- * Settings
    , GameParam(..)
    , framePerSecond
    , windowSize
    , windowTitle
    , windowed
    , cursorVisible
    , clearColor
    , windowOrigin

) where

import Control.Monad.Free
import Control.Monad.Free.Church
import Control.Monad
import Control.Monad.IO.Class
import Control.Lens
import Graphics.UI.FreeGame.Data.Color
import Graphics.UI.FreeGame.Data.Bitmap
import Graphics.UI.FreeGame.Input
import Graphics.UI.FreeGame.Internal.Finalizer
import Linear
import Data.Void
import Data.Default

infixr 5 `Translate`
infixr 5 `Rotate`
infixr 5 `Scale`
infixr 5 `Colored`

-- | 'Game' is a 'Monad' that abstracts user interfaces.

instance MonadFree (UI i o) m => MonadIO m where
    liftIO = wrap . EmbedIO . liftM return

instance MonadFree (UI i o) m => MonadState UIParam m where
    get = wrap $ GetGameParam return
    put x = wrap $ PutGameParam x (return ())

instance MonadFree (UI i o) m => MonadReader i m where
    ask = wrap $ Input return

-- | A base for 'Game' monad.
data UI i o a
    = Tick a
    | EmbedIO (IO a)
    | Bracket (Game a)
    | Quit
    | Draw o a
    | Input (i -> a)
    | GetUIParam (UIParam -> a)
    | PutUIParam UIParam a
    deriving Functor

-- | Finalize the current frame and refresh the screen.
tick :: MonadFree (UI i o) m => m ()
tick = wrap $ Tick (return ())

-- | Run a Game monad in a Game monad. resources (e.g. pictures) will be released when inner computation is done.
bracket :: MonadFree (UI i o) m => Game a -> m a
bracket m = wrap $ Bracket $ liftM return m

-- | Break the current computation.
quitGame :: MonadFree (UI i o) m => m Void
quitGame = wrap QuitGame

-- | Draw a 'Picture'.
draw :: MonadFree (UI i o) m => o -> m ()
draw pic = wrap $ Draw pic (return ())

-- | Lift a picture transformation into transformation of 'UI'
transPicture :: (Picture -> Picture) -> UI i o cont -> UI i o cont
transPicture f (DrawPicture p cont) = DrawPicture (f p) cont
transPicture f (Bracket m) = Bracket $ transPicture f m
transPicture _ x = x

-- | A 2D Picture.
data Picture
    -- | A 'Bitmap' as a 'Picture'.
    = Bitmap Bitmap
    -- | A picture consist of some 'Picture's.
    | Pictures [Picture]
    -- | A picture that may have side effects(internal use only).
    | PictureWithFinalizer (FinalizerT IO Picture)
    -- | Rotated picture by the given angle (in degrees, counterclockwise).
    | Rotate Float Picture
    -- | Scaled picture.
    | Scale V2 Picture
    -- | A picture translated by the given coordinate.
    | Translate V2 Picture
    -- | Colored picture.
    | Colored Color Picture

-- | Parameters of the application.
data UIParam = UIParam
    { _framePerSecond :: Int
    , _windowSize :: (Int, Int)
    , _windowTitle :: String
    , _windowed :: Bool
    , _cursorVisible :: Bool
    , _clearColor :: Color
    , _windowOrigin :: V2
    } deriving Show

-- | 640*480(windowed), 60fps
instance Default UIParam where
    def = UIParam
        { _framePerSecond = 60
        , _windowSize = (640, 480)
        , _windowTitle = "free-game"
        , _windowed = True
        , _cursorVisible = True
        , _clearColor = white
        , _windowOrigin = V2 0 0
        }
