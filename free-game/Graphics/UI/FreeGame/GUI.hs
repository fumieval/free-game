{-# LANGUAGE DeriveFunctor, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
module Graphics.UI.FreeGame.GUI (
    GUI
    , GUIBase(..)
    , GUIInput(..)
    , Picture(..)
    , GUIParam(..)
) where

import Graphics.UI.FreeGame.Base
import Graphics.UI.FreeGame.Types
import Graphics.UI.FreeGame.Data.Color
import Graphics.UI.FreeGame.Data.Bitmap
import Graphics.UI.FreeGame.Internal.Finalizer
import Control.Applicative
import Control.Applicative.Free (Ap)
import Control.Monad.Free.Church
import Control.Monad.Free.Class
import Data.Default
import Linear hiding (rotate)
import Control.Lens


type GUI = UI GUIParam GUIBase

data GUIBase a = Input (Ap GUIInput a) | Draw (Picture a) deriving Functor

_Draw :: Applicative f => (Picture a -> f (Picture a)) -> GUIBase a -> f (GUIBase a)
_Draw f (Draw o) = fmap Draw (f o)
_Draw _ x = pure x

instance Picture2D GUIBase where
    fromBitmap = Draw . fromBitmap
    withFinalizer = Draw . withFinalizer
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


data GUIInput a = 
      ICharKey Char (Bool -> a)
    | ISpecialKey SpecialKey (Bool -> a)
    | IMousePosition (V2 Float -> a)
    | IMouseWheel (Int -> a)
    | IMouseButtonL (Bool -> a)
    | IMouseButtonM (Bool -> a)
    | IMouseButtonR (Bool -> a)
    deriving Functor

-- | a 2D Picture.
data Picture a
    = LiftBitmap Bitmap a
    -- | A picture that may have side effects(internal use only).
    | PictureWithFinalizer (FinalizerT IO a)
    -- | Rotated picture by the given angle (in degrees,counterclockwise).
    | Rotate Float (Picture a)
    -- | Scaled picture.
    | Scale (V2 Float) (Picture a)
    -- | A picture translated by the given coordinate.
    | Translate (V2 Float) (Picture a)
    -- | Colored picture.
    | Colored Color (Picture a)
    deriving Functor

instance Picture2D Picture where
    fromBitmap = flip LiftBitmap ()
    withFinalizer = PictureWithFinalizer
    rotate = Rotate
    scale = Scale
    translate = Translate
    colored = Colored

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
        , _clearColor = white
        , _windowOrigin = V2 0 0
        }