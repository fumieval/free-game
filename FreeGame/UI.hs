{-# LANGUAGE DeriveFunctor, ExistentialQuantification #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.FreeGame.GUI
-- Copyright   :  (C) 2013 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
-- Provides the "free" embodiment.
----------------------------------------------------------------------------
module Graphics.UI.FreeGame.UI (
    UI(..)
    , GUIParam(..)
) where

import Graphics.UI.FreeGame.Base
import Graphics.UI.FreeGame.Data.Bitmap
import Graphics.UI.FreeGame.Internal.Finalizer
import Graphics.UI.FreeGame.Internal.Raindrop
import Control.Applicative
import Data.Default
import Data.Color

data UI a = Draw (forall m. (Monad m, Picture2D m) => m a)
    | FromFinalizer (FinalizerT IO a)
    | KeyState Key (Bool -> a)
    | MousePosition (Vec2 -> a)
    | MouseWheel (Int -> a)
    | MouseButtonL (Bool -> a)
    | MouseButtonM (Bool -> a)
    | MouseButtonR (Bool -> a)
    | Play Wave a

_Draw :: Applicative f => (forall m. (Monad m, Picture2D m) => m a -> f (m a)) -> UI a -> f (UI a)
_Draw f (Draw m) = fmap Draw (f m)
_Draw f x = pure x

instance Affine UI where
    translate v = over _Draw (translate v)
    rotateR t = over _Draw (rotateR t)
    rotateD t = over _Draw (rotateD t)
    scale v = over _Draw (scale v)

instance Picture2D UI where
    bitmap x = Draw (bitmap x)
    line vs = Draw (line vs)
    polygon vs = Draw (polygon vs)
    polygonOutline vs = Draw (polygonOutline vs)
    circle r = Draw (circle r)
    circleOutline r = Draw (circleOutline r)
    thickness t = over _Draw (thickness t)
    color c = over _Draw (color c)

instance Local UI where
    getViewPort = Draw getViewPort

instance FromFinalizer UI where
    fromFinalizer = PictureWithFinalizer

instance Keyboard UI where
    keyState x = KeyState x id

instance Mouse UI where
    mousePosition = MousePosition id
    mouseWheel = MouseWheel id
    mouseButtonL = MouseButtonL id
    mouseButtonR = MouseButtonR id
    mouseButtonM = MouseButtonM id

-- | Parameters of the application.
data GUIParam = GUIParam
    { _framePerSecond :: Int
    , _windowTitle :: String
    , _windowed :: Bool
    , _cursorVisible :: Bool
    , _clearColor :: Color
    , _windowRegion :: BoundingBox Double
    } deriving Show

instance Default GUIParam where
    def = GUIParam
        { _framePerSecond = 60
        , _windowTitle = "free-game"
        , _windowed = True
        , _cursorVisible = True
        , _clearColor = Color 1 1 1 1
        , _windowRegion = V2 0 0 640 480
        }