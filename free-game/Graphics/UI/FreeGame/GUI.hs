{-# LANGUAGE DeriveFunctor, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
module Graphics.UI.FreeGame.GUI (
    Game
    ,GUI
    , GUIInput(..)
    , Picture(..)
    , GUIParam(..)
) where

import Graphics.UI.FreeGame.Base
import Graphics.UI.FreeGame.Types
import Control.Applicative.Free (Ap)
import Control.Monad.Free.Church
import Control.Monad.Free.Class
import Data.Default
import Linear

type Game = F GUI

type GUI = UI GUIParam (Ap GUIInput) Picture

data GUIInput a = ICharKey Char (Bool -> a)
    | ISpecialKey SpecialKey (Bool -> a)
    | IMousePosition (V2 Float -> a)
    | IMouseWheel (Int -> a)
    | IMouseButtonL (Bool -> a)
    | IMouseButtonM (Bool -> a)
    | IMouseButtonR (Bool -> a)
    deriving Functor

instance KeyChar GUIInput where
    keyChar x = ICharKey x id

instance KeySpecial GUIInput where
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