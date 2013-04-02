{-# LANGUAGE DeriveFunctor #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.FreeGame.Types
-- Copyright   :  (C) 2013 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinsohita <fumiexcel@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Graphics.UI.FreeGame.Types (
    BoundingBox(..),
    Picture(..),
    KeyChar(..),
    KeySpecial(..),
    Mouse(..),
    SpecialKey(..),
    module Graphics.UI.FreeGame.Data.Color
    ) where

import Linear
import Data.Monoid
import Control.Applicative.Free
import Graphics.UI.FreeGame.Data.Bitmap
import Graphics.UI.FreeGame.Data.Color
import Graphics.UI.FreeGame.Internal.Finalizer

infixr 5 `Translate`
infixr 5 `Rotate`
infixr 5 `Scale`
infixr 5 `Colored`

-- | FIXME: this should inherit more classes
data BoundingBox a = BoundingBox (V2 a) (V2 a) deriving (Show, Eq, Ord, Functor, Read)

_topLeft :: Functor f => (V2 a -> f (V2 a)) -> (BoundingBox a -> f (BoundingBox a))
_topLeft f (BoundingBox a b) = fmap (`BoundingBox` b) (f a)

_bottomRight :: Functor f => (V2 a -> f (V2 a)) -> (BoundingBox a -> f (BoundingBox a))
_bottomRight f (BoundingBox a b) = fmap (a `BoundingBox`) (f b)

-- | A 2D Picture.
data Picture
    -- | A 'Bitmap' as a 'Picture'.
    = Bitmap Bitmap
    -- | A picture consist of some 'Picture's.
    | Pictures [Picture]
    -- | A picture that may have side effects(internal use only).
    | PictureWithFinalizer (FinalizerT IO Picture)
    -- | Rotated picture by the given angle (in degrees, counterclockwise).
    | Rotate Float Picture
    -- | Scaled picture.
    | Scale (V2 Float) Picture
    -- | A picture translated by the given coordinate.
    | Translate (V2 Float) Picture
    -- | Colored picture.
    | Colored Color Picture

instance Monoid Picture where
    mempty = Pictures []
    mappend x y = Pictures [x, y]

class KeyChar t where
    keyChar :: Char -> t Bool

class KeySpecial t where
    keySpecial :: SpecialKey -> t Bool

class Mouse t where
    mousePosition :: t (V2 Float)
    mouseWheel :: t Int
    mouseButtonL :: t Bool
    mouseButtonM :: t Bool
    mouseButtonR :: t Bool

instance KeyChar t => KeyChar (Ap t) where
    keyChar = liftAp . keyChar

instance KeySpecial t => KeySpecial (Ap t) where
    keySpecial = liftAp . keySpecial

instance Mouse t => Mouse (Ap t) where
    mousePosition = liftAp mousePosition
    mouseWheel = liftAp mouseWheel
    mouseButtonL = liftAp mouseButtonL
    mouseButtonR = liftAp mouseButtonR
    mouseButtonM = liftAp mouseButtonR

data SpecialKey = KeySpace
    | KeyEsc
    | KeyLeftShift
    | KeyRightShift
    | KeyLeftControl
    | KeyRightControl
    | KeyUp
    | KeyDown
    | KeyLeft
    | KeyRight
    | KeyTab
    | KeyEnter
    | KeyBackspace
    | KeyInsert
    | KeyDelete
    | KeyPageUp
    | KeyPageDown
    | KeyHome
    | KeyEnd
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
    deriving (Show, Eq, Ord, Enum)