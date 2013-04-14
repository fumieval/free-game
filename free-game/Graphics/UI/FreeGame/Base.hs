{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.FreeGame.Base
-- Copyright   :  (C) 2012-2013 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinsohita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Abstract structures that represents user interfaces
----------------------------------------------------------------------------

module Graphics.UI.FreeGame.Base (
    -- * Types
    UI(..)

    -- * Basic operations
    ,tick
    ,bracket
    ,quit
    ,embedIO
    ,liftUI
    ,_LiftUI
    -- * Classes
    ,Picture2D(..)
    ,Keyboard(..)
    ,Mouse(..) 
    ,SpecialKey(..)
) where

import Control.Applicative
import Control.Applicative.Free as Ap
import Control.Lens
import Control.Monad.Free.Church
import Control.Monad.Free.Class
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Data.Monoid
import Data.Void
import Graphics.UI.FreeGame.Data.Bitmap
import Graphics.UI.FreeGame.Data.Color
import Graphics.UI.FreeGame.Internal.Finalizer
import Linear hiding (rotate)
import qualified Control.Monad.Free as Free

infixr 5 `translate`
infixr 5 `rotate`
infixr 5 `scale`
infixr 5 `colored`

instance (Functor m) => MonadIO (F (UI p m)) where
    liftIO = embedIO

instance (Functor m) => MonadState p (F (UI p m)) where
    get = wrap $ GetParam return
    put x = wrap $ PutParam x (return ())

instance (Functor m) => MonadIO (Free.Free (UI p m)) where
    liftIO = embedIO

instance (Functor m) => MonadState p (Free.Free (UI p m)) where
    get = wrap $ GetParam return
    put x = wrap $ PutParam x (return ())

data UI p m a
    = Tick a
    | EmbedIO (IO a)
    | LiftUI (m a)
    | Bracket (F (UI p m) a)
    | Quit
    | GetParam (p -> a)
    | PutParam p a
    deriving Functor

-- | Finalize the current frame and refresh the screen.
tick :: MonadFree (UI p n) m => m ()
tick = wrap $ Tick (return ())

-- | Run a Game monad in a Game monad. resources (e.g. pictures) will be released when inner computation is done.
bracket :: MonadFree (UI p n) m => F (UI p n) a -> m a
bracket m = wrap $ Bracket $ fmap return m

-- | Break the entire computation.
quit :: MonadFree (UI p n) m => m Void
quit = wrap Quit

liftUI :: (Functor n, MonadFree (UI p n) m) => n a -> m a
liftUI = wrap . LiftUI . fmap return

embedIO :: (MonadFree (UI p n) m) => IO a -> m a
embedIO = wrap . EmbedIO . fmap return

_LiftUI :: Applicative f => (m cont -> f (m cont)) -> UI p m cont -> f (UI p m cont)
_LiftUI f (LiftUI o) = fmap LiftUI (f o)
_LiftUI _ x = pure x

class Picture2D p where
    fromBitmap :: Bitmap -> p ()
    withFinalizer :: FinalizerT IO a -> p a
    rotate :: Float -> p a -> p a
    scale :: V2 Float -> p a -> p a
    translate :: V2 Float -> p a -> p a
    colored :: Color -> p a -> p a

instance (Picture2D m) => Picture2D (UI param m) where
    fromBitmap = LiftUI . fromBitmap
    withFinalizer = LiftUI . withFinalizer
    rotate = over _LiftUI . rotate
    scale = over _LiftUI . scale
    translate = over _LiftUI . translate
    colored = over _LiftUI . colored

instance (Functor f, Picture2D f) => Picture2D (F f) where
    fromBitmap = liftF . fromBitmap
    withFinalizer = liftF . withFinalizer
    rotate = hoistF' . rotate
    scale = hoistF' . scale
    translate = hoistF' . translate
    colored = hoistF' . colored

instance (Functor f, Picture2D f) => Picture2D (Free.Free f) where
    fromBitmap = Free.liftF . fromBitmap
    withFinalizer = Free.liftF . withFinalizer
    rotate = hoistFree' . rotate
    scale = hoistFree' . scale
    translate = hoistFree' . translate
    colored = hoistFree' . colored

hoistFree' :: (Functor f, MonadFree g m) => (f (m a) -> g (m a)) -> Free.Free f a -> m a
hoistFree' t (Free.Pure a) = return a
hoistFree' t (Free.Free f) = wrap . t $ fmap (hoistFree' t) f
{-# INLINE hoistFree' #-}

hoistF' :: MonadFree g m => (f (m a) -> g (m a)) -> F f a -> m a
hoistF' t (F m) = m return (wrap . t)
{-# INLINE hoistF' #-}

class Keyboard t where
    keyChar :: Char -> t Bool
    keySpecial :: SpecialKey -> t Bool

class Mouse t where
    mousePosition :: t (V2 Float)
    mouseWheel :: t Int
    mouseButtonL :: t Bool
    mouseButtonM :: t Bool
    mouseButtonR :: t Bool

instance Keyboard t => Keyboard (Ap t) where
    keyChar = liftAp . keyChar
    keySpecial = liftAp . keySpecial

instance Mouse t => Mouse (Ap t) where
    mousePosition = liftAp mousePosition
    mouseWheel = liftAp mouseWheel
    mouseButtonL = liftAp mouseButtonL
    mouseButtonR = liftAp mouseButtonR
    mouseButtonM = liftAp mouseButtonR

instance Keyboard m => Keyboard (UI p m) where
    keyChar = LiftUI . keyChar
    keySpecial = LiftUI . keySpecial

instance Mouse m => Mouse (UI p m) where
    mousePosition = LiftUI mousePosition
    mouseWheel = LiftUI mouseWheel
    mouseButtonL = LiftUI mouseButtonL
    mouseButtonR = LiftUI mouseButtonR
    mouseButtonM = LiftUI mouseButtonR

instance (Functor f, Keyboard f) => Keyboard (F f) where
    keyChar = liftF . keyChar
    keySpecial = liftF . keySpecial

instance (Functor f, Mouse f) => Mouse (F f) where
    mousePosition = liftF mousePosition
    mouseWheel = liftF mouseWheel
    mouseButtonL = liftF mouseButtonL
    mouseButtonR = liftF mouseButtonR
    mouseButtonM = liftF mouseButtonR

instance (Functor f, Keyboard f) => Keyboard (Free.Free f) where
    keyChar = Free.liftF . keyChar
    keySpecial = Free.liftF . keySpecial

instance (Functor f, Mouse f) => Mouse (Free.Free f) where
    mousePosition = Free.liftF mousePosition
    mouseWheel = Free.liftF mouseWheel
    mouseButtonL = Free.liftF mouseButtonL
    mouseButtonR = Free.liftF mouseButtonR
    mouseButtonM = Free.liftF mouseButtonR

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
