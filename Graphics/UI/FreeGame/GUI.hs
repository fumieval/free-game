{-# LANGUAGE DeriveFunctor #-}
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
import Linear hiding (rotate)

data GUI x where
    | LiftBitmap Bitmap a
    | PictureWithFinalizer (FinalizerT IO a)
    | RotateD Float (F GUI a)
    | Scale (V2 Float) (F GUI a)
    | Translate (V2 Float) (F GUI a)
    | Colored Color (F GUI a)
    | Line [V2 Float] a
    | Polygon [V2 Float] a
    | PolygonOutline [V2 Float] a
    | Circle Float a
    | CircleOutline Float a
    | Thickness Float (Picture a)
    | ICharKey Char (Bool -> a)
    | ISpecialKey SpecialKey (Bool -> a)
    | IMousePosition (V2 Float -> a)
    | IMouseWheel (Int -> a)
    | IMouseButtonL (Bool -> a)
    | IMouseButtonM (Bool -> a)
    | IMouseButtonR (Bool -> a)
 
instance Picture2D Picture where
    fromBitmap = flip LiftBitmap ()
    rotateD = RotateD
    scale = Scale
    translate = Translate
    colored = Colored

instance Figure2D Picture where
    line = flip Line ()
    polygon = flip Polygon ()
    polygonOutline = flip PolygonOutline ()
    circle = flip Circle ()
    circleOutline = flip CircleOutline ()
    thickness = Thickness

instance FromFinalizer Picture where
    fromFinalizer = PictureWithFinalizer

instance Keyboard GUIInput where
    keyChar x = ICharKey x id
    keySpecial x = ISpecialKey x id

instance Mouse GUIInput where
    mousePosition = IMousePosition id
    mouseWheel = IMouseWheel id
    mouseButtonL = IMouseButtonL id
    mouseButtonR = IMouseButtonR id
    mouseButtonM = IMouseButtonM id

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