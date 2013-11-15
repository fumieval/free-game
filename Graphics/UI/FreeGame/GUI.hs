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
module Graphics.UI.FreeGame.Free (
    GUI
    , GUIBase(..)
    , _Draw
    , _Input
    , GUIInput(..)
    , Picture(..)
    , GUIParam(..)
) where

import Graphics.UI.FreeGame.Base
import Graphics.UI.FreeGame.Data.Bitmap
import Graphics.UI.FreeGame.Internal.Finalizer
import Graphics.UI.FreeGame.Internal.Raindrop
import Control.Applicative
import Data.Default
import Data.Color

data GUI a = Draw (forall m. (Monad m, Picture2D m) => m a)
    | FromFinalizer (FinalizerT IO a)
    | KeyState Key (Bool -> a)
    | MousePosition (Vec2 -> a)
    | MouseWheel (Int -> a)
    | MouseButtonL (Bool -> a)
    | MouseButtonM (Bool -> a)
    | MouseButtonR (Bool -> a)

cloneGUI :: (Picture2D f, Figure2D f, Keyboard f, Mouse f, FromFinalizer f, Functor f) => GUI a -> f a
cloneGUI (FromBitmap bmp a) = a <$ fromBitmap bmp
cloneGUI (FromFinalizer m) = fromFinalizer m
cloneGUI (KeyChar ch cont) = cont <$> keyChar ch
cloneGUI (KeySpecial ch cont) = cont <$> keySpecial ch
cloneGUI (MousePosition cont) = cont <$> mousePosition
cloneGUI (MouseWheel cont) = cont <$> mouseWheel
cloneGUI (MouseButtonL cont) = cont <$> mouseButtonL
cloneGUI (MouseButtonM cont) = cont <$> mouseButtonM
cloneGUI (MouseButtonR cont) = cont <$> mouseButtonR

instance Picture2D GUI where
    fromBitmap = flip LiftBitmap ()
    rotateD = RotateD
    scale = Scale
    translate = Translate
    colored = Colored

instance Figure2D GUI where
    line = flip Line ()
    polygon = flip Polygon ()
    polygonOutline = flip PolygonOutline ()
    circle = flip Circle ()
    circleOutline = flip CircleOutline ()
    thickness = Thickness

instance FromFinalizer GUI where
    fromFinalizer = PictureWithFinalizer

instance Keyboard GUI where
    keyChar x = ICharKey x id
    keySpecial x = ISpecialKey x id

instance Mouse GUI where
    mousePosition = MousePosition id
    mouseWheel = MouseWheel id
    mouseButtonL = MouseButtonL id
    mouseButtonR = MouseButtonR id
    mouseButtonM = MouseButtonM id

-- | Parameters of the application.
data GUIParam = GUIParam
    { _framePerSecond :: Int
    , _windowSize :: V2 Int
    , _windowTitle :: String
    , _windowed :: Bool
    , _cursorVisible :: Bool
    , _clearColor :: Color
    , _windowOrigin :: V2 Double
    } deriving Show

instance Default GUIParam where
    def = GUIParam
        { _framePerSecond = 60
        , _windowSize = V2 640 480
        , _windowTitle = "free-game"
        , _windowed = True
        , _cursorVisible = True
        , _clearColor = Color 1 1 1 1
        , _windowOrigin = V2 0 0
        }