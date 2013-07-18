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
import Linear hiding (rotate)

data GUI a = FromBitmap Bitmap a
    | FromFinalizer (FinalizerT IO a)
    | RotateD Float (GUI a)
    | Scale (V2 Float) (GUI a)
    | Translate (V2 Float) (GUI a)
    | Colored Color (GUI a)
    | Line [V2 Float] a
    | Polygon [V2 Float] a
    | PolygonOutline [V2 Float] a
    | Circle Float a
    | CircleOutline Float a
    | Thickness Float (GUI a)
    | KeyChar Char (Bool -> a)
    | KeySpecial SpecialKey (Bool -> a)
    | MousePosition (V2 Float -> a)
    | MouseWheel (Int -> a)
    | MouseButtonL (Bool -> a)
    | MouseButtonM (Bool -> a)
    | MouseButtonR (Bool -> a)
 
cloneGUI :: (Picture2D f, Figure2D f, Keyboard f, Mouse f, FromFinalizer f, Functor f) => GUI a -> f a
cloneGUI (FromBitmap bmp a) = a <$ fromBitmap bmp
cloneGUI (FromFinalizer m) = fromFinalizer m
cloneGUI (RotateD f m) = rotateD (cloneGUI m)
cloneGUI (Scale m) = scale (cloneGUI m)
cloneGUI (Translate m) = translate (cloneGUI m)
cloneGUI (Colored m) = colored (cloneGUI m)
cloneGUI (Line vs a) = a <$ line vs
cloneGUI (Polygon vs a) = a <$ polygon vs
cloneGUI (PolygonOutline vs a) = a <$ line vs
cloneGUI (Circle r a) = a <$ circle vs
cloneGUI (CircleOutline r a) = a <$ circleOutline r
cloneGUI (Thickness f m) = thickness f (cloneGUI m)
cloneGUI (KeyChar ch cont) = cont <$> keyChar ch
cloneGUI (KeySpecial ch cont) = cont <$> keySpecial ch
cloneGUI (MousePosition cont) = cont <$> mousePosition
cloneGUI (MouseWheel cont) = cont <$> mouseWheel
cloneGUI (MouseButtonL cont) = cont <$> mouseButtonL
cloneGUI (MouseButtonM cont) = cont <$> mouseButtonM
cloneGUI (MouseButtonR cont) = cont <$> mouseButtonR

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