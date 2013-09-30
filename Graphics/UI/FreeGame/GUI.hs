{-# LANGUAGE DeriveFunctor, BangPatterns #-}
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
module Graphics.UI.FreeGame.GUI (
    GUI
    , GUIBase(..)
    , GUIParam(..)
) where

import Graphics.UI.FreeGame.Base
import Graphics.UI.FreeGame.Data.Bitmap
import Graphics.UI.FreeGame.Internal.Finalizer
import Graphics.UI.FreeGame.Types
import Data.Default
import Data.Color

-- | A 'Functor' which represents graphical user interfaces.
type GUI = UI GUIBase

-- | The base of 'GUI'.
data GUIBase a = FromBitmap Bitmap a
    | FromFinalizer (FinalizerT IO a)
    | RotateD Float (GUIBase a)
    | Scale Vec2 (GUIBase a)
    | Translate Vec2 (GUIBase a)
    | Colored Color (GUIBase a)
    | Line [Vec2] a
    | Polygon [Vec2] a
    | PolygonOutline [Vec2] a
    | Circle Float a
    | CircleOutline Float a
    | Thickness Float (GUIBase a)
    | KeyChar Char (Bool -> a)
    | KeySpecial SpecialKey (Bool -> a)
    | MousePosition (Vec2 -> a)
    | MouseWheel (Int -> a)
    | MouseButtonL (Bool -> a)
    | MouseButtonM (Bool -> a)
    | MouseButtonR (Bool -> a)
    deriving Functor

instance Picture2D GUIBase where
    fromBitmap b = FromBitmap b ()
    rotateD = RotateD
    scale = Scale
    translate = Translate
    colored = Colored

instance Figure2D GUIBase where
    line = flip Line ()
    polygon = flip Polygon ()
    polygonOutline = flip PolygonOutline ()
    circle = flip Circle ()
    circleOutline = flip CircleOutline ()
    thickness = Thickness

instance Keyboard GUIBase where
    keyChar x = KeyChar x id
    keySpecial x = KeySpecial x id

instance Mouse GUIBase where
    mousePosition = MousePosition id
    mouseWheel = MouseWheel id
    mouseButtonL = MouseButtonL id
    mouseButtonR = MouseButtonR id
    mouseButtonM = MouseButtonM id

instance FromFinalizer GUIBase where
    fromFinalizer = FromFinalizer

-- | Parameters of the application.
data GUIParam = GUIParam
    { _framePerSecond :: Int
    , _windowRegion :: BoundingBox Float
    , _windowTitle :: String
    , _windowed :: Bool
    , _cursorVisible :: Bool
    , _clearColor :: Color
    } deriving Show

instance Default GUIParam where
    def = GUIParam
        { _framePerSecond = 60
        , _windowTitle = "free-game"
        , _windowed = True
        , _cursorVisible = True
        , _clearColor = Color 1 1 1 1
        , _windowRegion = BoundingBox 0 0 640 480
        }