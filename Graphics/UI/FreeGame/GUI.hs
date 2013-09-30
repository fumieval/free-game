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
    , _Draw
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
import Linear hiding (rotate)

-- | A 'Functor' which represents graphical user interfaces.
type GUI = UI GUIBase

-- | The base of 'GUI'.
data GUIBase a = FromBitmap Bitmap a
    | FromFinalizer (FinalizerT IO a)
    | RotateD Float (Picture a)
    | Scale (V2 Float) (Picture a)
    | Translate (V2 Float) (Picture a)
    | Colored Color (Picture a)
    | Line [V2 Float] a
    | Polygon [V2 Float] a
    | PolygonOutline [V2 Float] a
    | Circle Float a
    | CircleOutline Float a
    | Thickness Float (Picture a)
    | KeyChar Char (Bool -> a)
    | KeySpecial SpecialKey (Bool -> a)
    | MousePosition (V2 Float) a
    | MouseWheel (Int -> a)
    | MouseButtonL (Bool -> a)
    | MouseButtonM (Bool -> a)
    | MouseButtonR (Bool -> a)
    deriving Functor

-- | _Draw :: Traversal' (GUIBase a) (Picture a)
_Draw :: Applicative f => (Picture a -> f (Picture a)) -> GUIBase a -> f (GUIBase a)
_Draw f (Draw o) = fmap Draw (f o)
_Draw _ x = pure x

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
    , _windowSize :: V2 Int
    , _windowTitle :: String
    , _windowed :: Bool
    , _cursorVisible :: Bool
    , _clearColor :: Color
    , _windowOrigin :: V2 Float
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