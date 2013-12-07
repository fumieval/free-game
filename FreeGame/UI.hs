{-# LANGUAGE DeriveFunctor, ExistentialQuantification, Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  FreeGame.UI
-- Copyright   :  (C) 2013 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
-- Provides the "free" embodiment.
----------------------------------------------------------------------------
module FreeGame.UI (
    UI(..)
) where

import FreeGame.Class
import FreeGame.Data.Bitmap
import FreeGame.Internal.Finalizer
import FreeGame.Internal.Raindrop
import FreeGame.Data.Bitmap
import FreeGame.Data.Wave
import FreeGame.Types
import Data.Color
import Control.Applicative
import Data.Default
import Data.Color
import Linear
import Unsafe.Coerce

data UI a =
    Draw (forall m. (Monad m, Picture2D m, Local m) => m a)
    | FromFinalizer (FinalizerT IO a)
    | KeyState Key (Bool -> a)
    | MousePosition (Vec2 -> a)
    | MouseWheel (Int -> a)
    | MouseButtonL (Bool -> a)
    | MouseButtonM (Bool -> a)
    | MouseButtonR (Bool -> a)
    | Play Wave a
    | Configure Configuration

overDraw :: (forall m. (Monad m, Picture2D m) => m a -> m a) -> UI a -> UI a
overDraw f (Draw m) = Draw (f m)
overDraw f x = x

instance Affine UI where
    translate v = overDraw (translate v)
    rotateR t = overDraw (rotateR t)
    rotateD t = overDraw (rotateD t)
    scale v = overDraw (scale v)

instance Picture2D UI where
    bitmap x = Draw (bitmap x)
    line vs = Draw (line vs)
    polygon vs = Draw (polygon vs)
    polygonOutline vs = Draw (polygonOutline vs)
    circle r = Draw (circle r)
    circleOutline r = Draw (circleOutline r)
    thickness t = overDraw (thickness t)
    colored c = overDraw (colored c)

instance Local UI where
    getViewPort = Draw getViewPort

instance FromFinalizer UI where
    fromFinalizer = FromFinalizer

instance Keyboard UI where
    keyState x = KeyState x id

instance Mouse UI where
    globalMousePosition = MousePosition id
    mouseWheel = MouseWheel id
    mouseButtonL = MouseButtonL id
    mouseButtonR = MouseButtonR id
    mouseButtonM = MouseButtonM id

