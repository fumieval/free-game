{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  FreeGame.Types
-- Copyright   :  (C) 2013 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module FreeGame.Types (
    WindowMode(..),
    Vec2,
    BoundingBox(..),
    inBoundingBox,
    _Corners, 
    _TopLeft,
    _TopRight,
    _BottomLeft,
    _BottomRight
    , Key(..)
    ) where

import Linear.V2
import Control.Applicative
import Data.Foldable
import Data.Traversable
import Data.Typeable

data WindowMode = Windowed | FullScreen

-- | 2D bounding box
data BoundingBox a = BoundingBox a a a a deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Read, Typeable)

type Vec2 = V2 Double

-- | Determine whether the given point is in the 'BoundingBox'.
inBoundingBox :: Ord a => V2 a -> BoundingBox a -> Bool
inBoundingBox (V2 x y) (BoundingBox x0 y0 x1 y1) = x0 <= x && x <= x1 && y0 <= y && y <= y1

-- | @'_Corners' :: Traversal' ('BoundingBox' a) ('V2' a)@
_Corners :: Applicative f => (V2 a -> f (V2 a)) -> (BoundingBox a -> f (BoundingBox a))
_Corners f (BoundingBox x0 y0 x1 y1) = go <$> f (V2 x0 y0) <*> f (V2 x1 y0) <*> f (V2 x1 y1) <*> f (V2 x0 y1) where
    go (V2 x0' _) (V2 _ y1') (V2 x2' _) (V2 _ y3') = BoundingBox x0' y1' x2' y3'

-- | @'_TopLeft' :: Lens' ('BoundingBox' a) ('V2' a)@
_TopLeft :: Functor f => (V2 a -> f (V2 a)) -> (BoundingBox a -> f (BoundingBox a))
_TopLeft f (BoundingBox x0 y0 x1 y1) = fmap (\(V2 x0' y0') -> BoundingBox x0' y0' x1 y1) (f (V2 x0 y0))

-- | @'_TopRight' :: Lens' ('BoundingBox' a) ('V2' a)@
_TopRight :: Functor f => (V2 a -> f (V2 a)) -> (BoundingBox a -> f (BoundingBox a))
_TopRight f (BoundingBox x0 y0 x1 y1) = fmap (\(V2 x1' y0') -> BoundingBox x0 y0' x1' y1) (f (V2 x1 y0))

-- | @'_BottomLeft' :: Lens' ('BoundingBox' a) ('V2' a)@
_BottomLeft :: Functor f => (V2 a -> f (V2 a)) -> (BoundingBox a -> f (BoundingBox a))
_BottomLeft f (BoundingBox x0 y0 x1 y1) = fmap (\(V2 x0' y1') -> BoundingBox x0' y0 x1 y1') (f (V2 x0 y1))

-- | @'_BottomRight' :: Lens' ('BoundingBox' a) ('V2' a)@
_BottomRight :: Functor f => (V2 a -> f (V2 a)) -> (BoundingBox a -> f (BoundingBox a))
_BottomRight f (BoundingBox x0 y0 x1 y1) = fmap (\(V2 x1' y1') -> BoundingBox x0 y0 x1' y1') (f (V2 x1 y1))

data Key =
      KeyUnknown
    | KeySpace
    | KeyApostrophe
    | KeyComma
    | KeyMinus
    | KeyPeriod
    | KeySlash
    | Key0
    | Key1
    | Key2
    | Key3
    | Key4
    | Key5
    | Key6
    | Key7
    | Key8
    | Key9
    | KeySemicolon
    | KeyEqual
    | KeyA
    | KeyB
    | KeyC
    | KeyD
    | KeyE
    | KeyF
    | KeyG
    | KeyH
    | KeyI
    | KeyJ
    | KeyK
    | KeyL
    | KeyM
    | KeyN
    | KeyO
    | KeyP
    | KeyQ
    | KeyR
    | KeyS
    | KeyT
    | KeyU
    | KeyV
    | KeyW
    | KeyX
    | KeyY
    | KeyZ
    | KeyLeftBracket
    | KeyBackslash
    | KeyRightBracket
    | KeyGraveAccent
    | KeyWorld1
    | KeyWorld2
    | KeyEscape
    | KeyEnter
    | KeyTab
    | KeyBackspace
    | KeyInsert
    | KeyDelete
    | KeyRight
    | KeyLeft
    | KeyDown
    | KeyUp
    | KeyPageUp
    | KeyPageDown
    | KeyHome
    | KeyEnd
    | KeyCapsLock
    | KeyScrollLock
    | KeyNumLock
    | KeyPrintScreen
    | KeyPause
    | KeyF1
    | KeyF2
    | KeyF3
    | KeyF4
    | KeyF5
    | KeyF6
    | KeyF7
    | KeyF8
    | KeyF9
    | KeyF10
    | KeyF11
    | KeyF12
    | KeyF13
    | KeyF14
    | KeyF15
    | KeyF16
    | KeyF17
    | KeyF18
    | KeyF19
    | KeyF20
    | KeyF21
    | KeyF22
    | KeyF23
    | KeyF24
    | KeyF25
    | KeyPad0
    | KeyPad1
    | KeyPad2
    | KeyPad3
    | KeyPad4
    | KeyPad5
    | KeyPad6
    | KeyPad7
    | KeyPad8
    | KeyPad9
    | KeyPadDecimal
    | KeyPadDivide
    | KeyPadMultiply
    | KeyPadSubtract
    | KeyPadAdd
    | KeyPadEnter
    | KeyPadEqual
    | KeyLeftShift
    | KeyLeftControl
    | KeyLeftAlt
    | KeyLeftSuper
    | KeyRightShift
    | KeyRightControl
    | KeyRightAlt
    | KeyRightSuper
    | KeyMenu
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Bounded Key where
    minBound = KeyUnknown
    maxBound = KeyMenu