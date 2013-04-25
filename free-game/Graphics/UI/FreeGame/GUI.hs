{-# LANGUAGE DeriveFunctor #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.FreeGame.GUI
-- Copyright   :  (C) 2013 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinsohita <fumiexcel@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
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
import Graphics.UI.FreeGame.Data.Color
import Graphics.UI.FreeGame.Data.Bitmap
import Graphics.UI.FreeGame.Internal.Finalizer
import Graphics.UI.FreeGame.Internal.Raindrop
import Control.Applicative
import Control.Applicative.Free (Ap)
import Data.Default
import Linear hiding (rotate)

type GUI = UI GUIBase

data GUIBase a = Input (Ap GUIInput a) | Draw (Picture a) deriving Functor

_Draw :: Applicative f => (Picture a -> f (Picture a)) -> GUIBase a -> f (GUIBase a)
_Draw f (Draw o) = fmap Draw (f o)
_Draw _ x = pure x

_Input :: Applicative f => (Ap GUIInput a -> f (Ap GUIInput a)) -> GUIBase a -> f (GUIBase a)
_Input f (Input o) = fmap Input (f o)
_Input _ x = pure x

instance Picture2D GUIBase where
    fromBitmap = Draw . fromBitmap
    rotate = over _Draw . rotate
    scale = over _Draw . scale
    translate = over _Draw . translate
    colored = over _Draw . colored

instance Keyboard GUIBase where
    keyChar = Input . keyChar
    keySpecial = Input . keySpecial

instance Mouse GUIBase where
    mousePosition = Input mousePosition
    mouseWheel = Input mouseWheel
    mouseButtonL = Input mouseButtonL
    mouseButtonR = Input mouseButtonR
    mouseButtonM = Input mouseButtonR

instance FromFinalizer GUIBase where
    fromFinalizer = Draw . fromFinalizer

data GUIInput a = 
      ICharKey Char (Bool -> a)
    | ISpecialKey SpecialKey (Bool -> a)
    | IMousePosition (V2 Float -> a)
    | IMouseWheel (Int -> a)
    | IMouseButtonL (Bool -> a)
    | IMouseButtonM (Bool -> a)
    | IMouseButtonR (Bool -> a)
    deriving Functor

data Picture a
    = LiftBitmap Bitmap a
    | PictureWithFinalizer (FinalizerT IO a)
    | Rotate Float (Picture a)
    | Scale (V2 Float) (Picture a)
    | Translate (V2 Float) (Picture a)
    | Colored Color (Picture a)
    deriving Functor

instance Picture2D Picture where
    fromBitmap = flip LiftBitmap ()
    rotate = Rotate
    scale = Scale
    translate = Translate
    colored = Colored

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
        , _clearColor = white
        , _windowOrigin = V2 0 0
        }