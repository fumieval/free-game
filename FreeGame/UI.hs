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
import FreeGame.Types
import Control.Applicative
import qualified Data.Map as Map
import FreeGame.Data.Bitmap (Bitmap)
import Data.Color
import Control.Monad.Free.Church
import Control.Monad.Trans.Iter

data UI a =
    Draw (forall m. (Applicative m, Monad m, Picture2D m, Local m) => m a)
    | PreloadBitmap Bitmap a
    | FromFinalizer (FinalizerT IO a)
    | KeyStates (Map.Map Key Bool -> Map.Map Key Bool -> a)
    | MouseButtons (Map.Map Int Bool -> Map.Map Int Bool -> a)
    | MousePosition (Vec2 -> a)
    | TakeScreenshot (Bitmap -> a)
    | Bracket (Frame a)
    | SetFPS Int a
    | SetTitle String a
    | ShowCursor a
    | HideCursor a
    | ClearColor Color a
    | GetFPS (Int -> a)
    | SetBlendMode Blending a
    deriving Functor

type Game = IterT Frame

type Frame = F UI

class (Picture2D m, Local m, Keyboard m, Mouse m, FromFinalizer m) => FreeGame m where
    -- | Draw an action that consist of 'Picture2D''s methods.
    draw :: (forall f. (Applicative f, Monad f, Picture2D f, Local f) => f a) => m a
    -- | Load a 'Bitmap' to avoid the cost of the first invocation of 'bitmap'.
    preloadBitmap :: Bitmap -> m ()
    -- | Run a 'Frame', and release all the matter happened.
    bracket :: Frame a -> m a
    -- | Generate a 'Bitmap' from the front buffer.
    takeScreenshot :: m Bitmap
    setFPS :: Int -> m ()
    setTitle :: String -> m ()
    showCursor :: m ()
    hideCursor :: m ()
    clearColor :: Color -> m ()
    getFPS :: m Int
    setBlendMode :: Blending -> m ()
    

instance FreeGame UI where
    draw = Draw
    {-# INLINE draw #-}
    preloadBitmap bmp = PreloadBitmap bmp ()
    {-# INLINE preloadBitmap #-}
    
    bracket = Bracket
    {-# INLINE bracket #-}
    takeScreenshot = TakeScreenshot id
    setFPS a = SetFPS a ()
    setTitle t = SetTitle t ()
    showCursor = ShowCursor ()
    hideCursor = HideCursor ()
    clearColor c = ClearColor c ()
    getFPS = GetFPS id
    setBlendMode bl = SetBlendMode bl ()
    

overDraw :: (forall m. (Applicative m, Monad m, Picture2D m, Local m) => m a -> m a) -> UI a -> UI a
overDraw f (Draw m) = Draw (f m)
overDraw _ x = x
{-# INLINE overDraw #-}

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