{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.FreeGame.Base
-- Copyright   :  (C) 2012-2013 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinsohita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Abstract structures for descripting user interfaces
----------------------------------------------------------------------------

module Graphics.UI.FreeGame.Base (
    -- * Types
    UI(..)

    -- * Basic operations
    ,tick
    ,bracket
    ,_Bracket
    ,quit
    ,embedIO
    ,_EmbedIO
    ,liftUI
    ,_LiftUI
    -- * Classes
    ,Picture2D(..)
    ,Figure2D(..)
    ,Keyboard(..)
    ,Mouse(..)
    ,FromFinalizer(..)
    ,SpecialKey(..)
) where

import Control.Applicative
import Control.Applicative.Free as Ap
import Control.Monad.IO.Class
import Data.Monoid
import Graphics.UI.FreeGame.Data.Bitmap
import Graphics.UI.FreeGame.Data.Wave
import Graphics.UI.FreeGame.Data.Color
import Graphics.UI.FreeGame.Internal.Finalizer
import Graphics.UI.FreeGame.Internal.Raindrop
import Linear hiding (rotate)

import Control.Monad.Free.Class
import Control.Monad.Free.Church
import qualified Control.Monad.Free as Free

import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.List
import Control.Monad.Trans.Error
import Control.Monad.Trans.Identity
import qualified Control.Monad.State.Lazy as Lazy
import qualified Control.Monad.State.Strict as Strict
import qualified Control.Monad.Writer.Lazy as Lazy
import qualified Control.Monad.Writer.Strict as Strict
import qualified Control.Monad.Trans.RWS.Strict as Strict
import qualified Control.Monad.Trans.RWS.Lazy as Lazy

infixr 5 `translate`
infixr 5 `rotate`
infixr 5 `scale`
infixr 5 `colored`

instance (Functor m) => MonadIO (F (UI m)) where
    liftIO = embedIO

instance (Functor m) => MonadIO (Free.Free (UI m)) where
    liftIO = embedIO

-- | A functor enriches given functor with control structure.
data UI m a
    = Tick a
    | EmbedIO (IO a)
    | LiftUI (m a)
    | Bracket (F (UI m) a)
    | Quit
    deriving Functor

-- | Finalize the current frame and refresh the screen.
tick :: MonadFree (UI n) m => m ()
tick = wrap $ Tick (return ())

-- | Run a Game monad in a Game monad. resources (e.g. pictures) will be released when inner computation is done.
bracket :: MonadFree (UI n) m => F (UI n) a -> m a
bracket = wrap . Bracket . fmap return

-- | Break the entire computation.
quit :: MonadFree (UI n) m => m a
quit = wrap Quit

-- | Lift 'UI''s base functor.
liftUI :: (Functor n, MonadFree (UI n) m) => n a -> m a
liftUI = wrap . LiftUI . fmap return

-- | Lift an arbitrary 'IO' action.
embedIO :: (MonadFree (UI n) m) => IO a -> m a
embedIO = wrap . EmbedIO . fmap return

-- | @'_EmbedIO' :: Traversal' ('UI' m a) (IO a)@
_EmbedIO :: Applicative f => (IO a -> f (IO a)) -> UI m a -> f (UI m a)
_EmbedIO f (EmbedIO m) = fmap EmbedIO (f m)
_EmbedIO _ x = pure x

-- | @'_Bracket' :: Traversal' ('UI' m a) (F (UI m) a)@
_Bracket :: Applicative f => (F (UI m) a -> f (F (UI m) a)) -> UI m a -> f (UI m a)
_Bracket f (Bracket m) = fmap Bracket (f m)
_Bracket _ x = pure x

-- | @'_LiftUI' :: Traversal' ('UI' m a) (m a)@
_LiftUI :: Applicative f => (m a -> f (m a)) -> UI m a -> f (UI m a)
_LiftUI f (LiftUI m) = fmap LiftUI (f m)
_LiftUI _ x = pure x

hoistFreeR :: (Functor f, MonadFree g m) => (f (m a) -> g (m a)) -> Free.Free f a -> m a
hoistFreeR _ (Free.Pure a) = return a
hoistFreeR t (Free.Free f) = wrap . t $ fmap (hoistFreeR t) f
{-# INLINE hoistFreeR #-}

hoistFR :: MonadFree g m => (f (m a) -> g (m a)) -> F f a -> m a
hoistFR t (F m) = m return (wrap . t)
{-# INLINE hoistFR #-}

-- | The class of types that can be regarded as a kind of picture.
class Picture2D p where
    -- | Construct a 'Picture2D' from a 'Bitmap'.
    fromBitmap :: Bitmap -> p ()
    -- | Counterclockwise, degrees
    rotate :: Float -> p a -> p a
    scale :: V2 Float -> p a -> p a
    translate :: V2 Float -> p a -> p a
    colored :: Color -> p a -> p a

class Picture2D p => Figure2D p where
    line :: [V2 Float] -> p ()
    polygon :: [V2 Float] -> p ()
    polygonOutline :: [V2 Float] -> p ()
    circle :: Float -> p ()
    circleOutline :: Float -> p ()
    thickness :: Float -> p a -> p a

class Sound p where
    fromWave :: Wave -> p ()
    volume :: Float -> p a -> p a
    pan :: Float -> p a -> p a

-- | The class of types that can handle inputs of the keyboard.
class Keyboard t where
    keyChar :: Char -> t Bool
    keySpecial :: SpecialKey -> t Bool

-- | The class of types that can handle inputs of the mouse.
class Mouse t where
    mousePosition :: t (V2 Float)
    mouseWheel :: t Int
    mouseButtonL :: t Bool
    mouseButtonM :: t Bool
    mouseButtonR :: t Bool

class FromFinalizer m where
    fromFinalizer :: FinalizerT IO a -> m a

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
    | KeyPadDivide
    | KeyPadMultiply
    | KeyPadSubtract
    | KeyPadAdd
    | KeyPadDecimal
    | KeyPadEqual
    | KeyPadEnter
    deriving (Show, Eq, Ord, Enum)

#define _COMMA_ ,

#define MK_PICTURE_2D(cxt, ty, l, t) instance (Picture2D m cxt) => Picture2D (ty) where { \
    fromBitmap = (l) . fromBitmap; \
    rotate = (t) . rotate; \
    translate = (t) . translate; \
    scale = (t) . scale; \
    colored = (t) . colored }

#define MK_FIGURE_2D(cxt, ty, l, t) instance (Figure2D m cxt) => Figure2D (ty) where { \
    line = (l) . line; \
    polygon = (l) . polygon; \
    polygonOutline = (l) . polygonOutline; \
    circle = (l) . circle; \
    circleOutline = (l) . circleOutline; \
    thickness = (t) . thickness }

#define MK_SOUND(cxt, ty, l, t) instance (Sound m cxt) => Sound (ty) where { \
    fromWave = (l) . fromWave; \
    volume = (t) . volume; \
    pan = (t) . pan }

#define MK_KEYBOARD(cxt, ty, l) instance (Keyboard m cxt) => Keyboard (ty) where { \
    keyChar = (l) . keyChar; \
    keySpecial = (l) . keySpecial }

#define MK_MOUSE(cxt, ty, l) instance (Mouse m cxt) => Mouse (ty) where { \
    mousePosition = (l) mousePosition; \
    mouseWheel = (l) mouseWheel; \
    mouseButtonL = (l) mouseButtonL; \
    mouseButtonR = (l) mouseButtonR; \
    mouseButtonM = (l) mouseButtonM }

#define MK_FROM_FINALIZER(cxt, ty, l) instance (FromFinalizer m cxt) => FromFinalizer (ty) where { \
    fromFinalizer = (l) . fromFinalizer }

MK_PICTURE_2D(_COMMA_ Functor m, F m, liftF, hoistFR)
MK_PICTURE_2D( , UI m, LiftUI, over _LiftUI)
MK_PICTURE_2D(_COMMA_ Functor m, Free.Free m, Free.liftF, hoistFreeR)
MK_PICTURE_2D(_COMMA_ Monad m, ReaderT r m, lift, mapReaderT)
MK_PICTURE_2D(_COMMA_ Monad m, Lazy.StateT s m, lift, Lazy.mapStateT)
MK_PICTURE_2D(_COMMA_ Monad m, Strict.StateT s m, lift, Strict.mapStateT)
MK_PICTURE_2D(_COMMA_ Monad m _COMMA_ Monoid w, Lazy.WriterT w m, lift, Lazy.mapWriterT)
MK_PICTURE_2D(_COMMA_ Monad m _COMMA_ Monoid w, Strict.WriterT w m, lift, Strict.mapWriterT)
MK_PICTURE_2D(_COMMA_ Monad m _COMMA_ Monoid w, Lazy.RWST r w s m, lift, Lazy.mapRWST)
MK_PICTURE_2D(_COMMA_ Monad m _COMMA_ Monoid w, Strict.RWST r w s m, lift, Strict.mapRWST)
MK_PICTURE_2D(_COMMA_ Monad m, IdentityT m, lift, mapIdentityT)
MK_PICTURE_2D(_COMMA_ Monad m, MaybeT m, lift, mapMaybeT)
MK_PICTURE_2D(_COMMA_ Monad m, ListT m, lift, mapListT)
MK_PICTURE_2D(_COMMA_ Monad m _COMMA_ Error e, ErrorT e m, lift, mapErrorT)
MK_PICTURE_2D(_COMMA_ Monad m, ContT r m, lift, mapContT)

MK_FIGURE_2D(_COMMA_ Functor m, F m, liftF, hoistFR)
MK_FIGURE_2D( , UI m, LiftUI, over _LiftUI)
MK_FIGURE_2D(_COMMA_ Functor m, Free.Free m, Free.liftF, hoistFreeR)
MK_FIGURE_2D(_COMMA_ Monad m, ReaderT r m, lift, mapReaderT)
MK_FIGURE_2D(_COMMA_ Monad m, Lazy.StateT s m, lift, Lazy.mapStateT)
MK_FIGURE_2D(_COMMA_ Monad m, Strict.StateT s m, lift, Strict.mapStateT)
MK_FIGURE_2D(_COMMA_ Monad m _COMMA_ Monoid w, Lazy.WriterT w m, lift, Lazy.mapWriterT)
MK_FIGURE_2D(_COMMA_ Monad m _COMMA_ Monoid w, Strict.WriterT w m, lift, Strict.mapWriterT)
MK_FIGURE_2D(_COMMA_ Monad m _COMMA_ Monoid w, Lazy.RWST r w s m, lift, Lazy.mapRWST)
MK_FIGURE_2D(_COMMA_ Monad m _COMMA_ Monoid w, Strict.RWST r w s m, lift, Strict.mapRWST)
MK_FIGURE_2D(_COMMA_ Monad m, IdentityT m, lift, mapIdentityT)
MK_FIGURE_2D(_COMMA_ Monad m, MaybeT m, lift, mapMaybeT)
MK_FIGURE_2D(_COMMA_ Monad m, ListT m, lift, mapListT)
MK_FIGURE_2D(_COMMA_ Monad m _COMMA_ Error e, ErrorT e m, lift, mapErrorT)
MK_FIGURE_2D(_COMMA_ Monad m, ContT r m, lift, mapContT)

MK_SOUND(_COMMA_ Functor m, F m, liftF, hoistFR)
MK_SOUND( , UI m, LiftUI, over _LiftUI)
MK_SOUND(_COMMA_ Functor m, Free.Free m, Free.liftF, hoistFreeR)
MK_SOUND(_COMMA_ Monad m, ReaderT r m, lift, mapReaderT)
MK_SOUND(_COMMA_ Monad m, Lazy.StateT s m, lift, Lazy.mapStateT)
MK_SOUND(_COMMA_ Monad m, Strict.StateT s m, lift, Strict.mapStateT)
MK_SOUND(_COMMA_ Monad m _COMMA_ Monoid w, Lazy.WriterT w m, lift, Lazy.mapWriterT)
MK_SOUND(_COMMA_ Monad m _COMMA_ Monoid w, Strict.WriterT w m, lift, Strict.mapWriterT)
MK_SOUND(_COMMA_ Monad m _COMMA_ Monoid w, Lazy.RWST r w s m, lift, Lazy.mapRWST)
MK_SOUND(_COMMA_ Monad m _COMMA_ Monoid w, Strict.RWST r w s m, lift, Strict.mapRWST)
MK_SOUND(_COMMA_ Monad m, IdentityT m, lift, mapIdentityT)
MK_SOUND(_COMMA_ Monad m, MaybeT m, lift, mapMaybeT)
MK_SOUND(_COMMA_ Monad m, ListT m, lift, mapListT)
MK_SOUND(_COMMA_ Monad m _COMMA_ Error e, ErrorT e m, lift, mapErrorT)
MK_SOUND(_COMMA_ Monad m, ContT r m, lift, mapContT)

MK_KEYBOARD(, Ap m, liftAp)
MK_KEYBOARD(, UI m, LiftUI)
MK_KEYBOARD(_COMMA_ Functor m, F m, liftF)
MK_KEYBOARD(_COMMA_ Functor m, Free.Free m, Free.liftF)
MK_KEYBOARD(_COMMA_ Monad m, ReaderT s m, lift)
MK_KEYBOARD(_COMMA_ Monad m, Lazy.StateT s m, lift)
MK_KEYBOARD(_COMMA_ Monad m, Strict.StateT s m, lift)
MK_KEYBOARD(_COMMA_ Monad m _COMMA_ Monoid w, Lazy.WriterT w m, lift)
MK_KEYBOARD(_COMMA_ Monad m _COMMA_ Monoid w, Strict.WriterT w m, lift)
MK_KEYBOARD(_COMMA_ Monad m _COMMA_ Monoid w, Lazy.RWST r w s m, lift)
MK_KEYBOARD(_COMMA_ Monad m _COMMA_ Monoid w, Strict.RWST r w s m, lift)
MK_KEYBOARD(_COMMA_ Monad m, IdentityT m, lift)
MK_KEYBOARD(_COMMA_ Monad m, MaybeT m, lift)
MK_KEYBOARD(_COMMA_ Monad m, ListT m, lift)
MK_KEYBOARD(_COMMA_ Monad m _COMMA_ Error e, ErrorT e m, lift)
MK_KEYBOARD(_COMMA_ Monad m, ContT r m, lift)

MK_MOUSE(, Ap m, liftAp)
MK_MOUSE(, UI m, LiftUI)
MK_MOUSE(_COMMA_ Functor m, F m, liftF)
MK_MOUSE(_COMMA_ Functor m, Free.Free m, Free.liftF)
MK_MOUSE(_COMMA_ Monad m, ReaderT r m, lift)
MK_MOUSE(_COMMA_ Monad m, Lazy.StateT s m, lift)
MK_MOUSE(_COMMA_ Monad m, Strict.StateT s m, lift)
MK_MOUSE(_COMMA_ Monad m _COMMA_ Monoid w, Lazy.WriterT w m, lift)
MK_MOUSE(_COMMA_ Monad m _COMMA_ Monoid w, Strict.WriterT w m, lift)
MK_MOUSE(_COMMA_ Monad m _COMMA_ Monoid w, Lazy.RWST r w s m, lift)
MK_MOUSE(_COMMA_ Monad m _COMMA_ Monoid w, Strict.RWST r w s m, lift)
MK_MOUSE(_COMMA_ Monad m, IdentityT m, lift)
MK_MOUSE(_COMMA_ Monad m, MaybeT m, lift)
MK_MOUSE(_COMMA_ Monad m, ListT m, lift)
MK_MOUSE(_COMMA_ Monad m _COMMA_ Error e, ErrorT e m, lift)
MK_MOUSE(_COMMA_ Monad m, ContT r m, lift)

MK_FROM_FINALIZER(, UI m, LiftUI)
MK_FROM_FINALIZER(_COMMA_ Functor m, F m, liftF)
MK_FROM_FINALIZER(_COMMA_ Functor m, Free.Free m, Free.liftF)
MK_FROM_FINALIZER(_COMMA_ Monad m, Lazy.StateT s m, lift)
MK_FROM_FINALIZER(_COMMA_ Monad m, Strict.StateT s m, lift)
MK_FROM_FINALIZER(_COMMA_ Monad m _COMMA_ Monoid w, Lazy.WriterT w m, lift)
MK_FROM_FINALIZER(_COMMA_ Monad m _COMMA_ Monoid w, Strict.WriterT w m, lift)
MK_FROM_FINALIZER(_COMMA_ Monad m _COMMA_ Monoid w, Lazy.RWST r w s m, lift)
MK_FROM_FINALIZER(_COMMA_ Monad m _COMMA_ Monoid w, Strict.RWST r w s m, lift)
MK_FROM_FINALIZER(_COMMA_ Monad m, IdentityT m, lift)
MK_FROM_FINALIZER(_COMMA_ Monad m, MaybeT m, lift)
MK_FROM_FINALIZER(_COMMA_ Monad m, ListT m, lift)
MK_FROM_FINALIZER(_COMMA_ Monad m _COMMA_ Error e, ErrorT e m, lift)
MK_FROM_FINALIZER(_COMMA_ Monad m, ContT r m, lift)