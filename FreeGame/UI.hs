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
    , Frame
    , Game
    , FreeGame(..)
) where

import FreeGame.Class
import FreeGame.Internal.Finalizer
import FreeGame.Data.Wave
import FreeGame.Types
import Control.Applicative
import qualified Data.Map as Map
import FreeGame.Data.Bitmap (Bitmap)

import Control.Monad.Free.Church
import Control.Monad.Trans.Iter

data UI a =
    Draw (forall m. (Applicative m, Monad m, Picture2D m, Local m) => m a)
    | PreloadBitmap Bitmap a
    | FromFinalizer (FinalizerT IO a)
    | KeyStates (Map.Map Key Bool -> Map.Map Key Bool -> a)
    | MouseButtons (Map.Map Int Bool -> Map.Map Int Bool -> a)
    | MousePosition (Vec2 -> a)
    | Play Wave a
    | Configure Configuration a
    | TakeScreenshot (Bitmap -> a)
    | Bracket (Frame a)
    deriving Functor

type Game = IterT Frame

type Frame = F UI

class (Picture2D m, Local m, Keyboard m, Mouse m, FromFinalizer m) => FreeGame m where
    -- | Draw an action that consist of 'Picture2D''s methods.
    draw :: (forall f. (Applicative f, Monad f, Picture2D f, Local f) => f a) => m a
    -- | Load a 'Bitmap' to avoid the cost of the first invocation of 'bitmap'.
    preloadBitmap :: Bitmap -> m ()
    -- | Apply a 'Configuration'.
    configure :: Configuration -> m ()
    -- | Generate a 'Bitmap' from the front buffer.
    takeScreenshot :: m Bitmap
    -- | Run a 'Frame', and release all the matter happened.
    bracket :: Frame a -> m a

instance FreeGame UI where
    draw = Draw
    {-# INLINE draw #-}
    preloadBitmap bmp = PreloadBitmap bmp ()
    {-# INLINE preloadBitmap #-}
    configure conf = Configure conf ()
    {-# INLINE configure #-}
    takeScreenshot = TakeScreenshot id
    {-# INLINE takeScreenshot #-}
    bracket = Bracket
    {-# INLINE bracket #-}

overDraw :: (forall m. (Applicative m, Monad m, Picture2D m, Local m) => m a -> m a) -> UI a -> UI a
overDraw f (Draw m) = Draw (f m)
overDraw _ x = x

instance Affine UI where
    translate v = overDraw (translate v)
    {-# INLINE translate #-}
    rotateR t = overDraw (rotateR t)
    {-# INLINE rotateR #-}
    rotateD t = overDraw (rotateD t)
    {-# INLINE rotateD #-}
    scale v = overDraw (scale v)
    {-# INLINE scale #-}

instance Picture2D UI where
    bitmap x = Draw (bitmap x)
    line vs = Draw (line vs)
    polygon vs = Draw (polygon vs)
    polygonOutline vs = Draw (polygonOutline vs)
    circle r = Draw (circle r)
    circleOutline r = Draw (circleOutline r)
    thickness t = overDraw (thickness t)
    color c = overDraw (color c)

instance Local UI where
    getLocation = Draw getLocation

instance FromFinalizer UI where
    fromFinalizer = FromFinalizer

instance Keyboard UI where
    keyStates_ = KeyStates (,)

instance Mouse UI where
    globalMousePosition = MousePosition id
    -- mouseWheel = MouseWheel id
    mouseButtons_ = MouseButtons (,)