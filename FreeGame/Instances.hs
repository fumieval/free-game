{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module FreeGame.Instances () where

import Control.Monad.Trans
import Control.Monad.Trans.Iter
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
import Data.Monoid
import Control.Monad.Free.Church as Church
import Control.Monad.Free as Free
import FreeGame.Class
import FreeGame.UI

#define _COMMA_ ,

#define MK_AFFINE(cxt, ty, t) instance (Affine m cxt) => Affine (ty) where { \
    rotateD a = t (rotateD a); \
    {-# INLINE rotateD #-}; \
    rotateR a = t (rotateR a); \
    {-# INLINE rotateR #-}; \
    translate a = t (translate a); \
    {-# INLINE translate #-}; \
    scale a = t (scale a); \
    {-# INLINE scale #-}; \
     }

#define MK_PICTURE_2D(cxt, ty, l, t) instance (Picture2D m cxt) => Picture2D (ty) where { \
    bitmap b = (l) (bitmap b); \
    {-# INLINE bitmap #-}; \
    line = (l) . line; \
    polygon = (l) . polygon; \
    polygonOutline = (l) . polygonOutline; \
    circle = (l) . circle; \
    circleOutline = (l) . circleOutline; \
    thickness k = t (thickness k); \
    color k = t (color k); \
    {-# INLINE color #-}; \
    blendMode m = t (blendMode m); \
    {-# INLINE blendMode #-}; \
    }

#define MK_LOCAL(cxt, ty, l) instance (Local m cxt) => Local (ty) where { \
    getLocation = (l) getLocation }

#define MK_KEYBOARD(cxt, ty, l) instance (Keyboard m cxt) => Keyboard (ty) where { \
    keyStates_ = (l) keyStates_; }

#define MK_MOUSE(cxt, ty, l) instance (Mouse m cxt) => Mouse (ty) where { \
    globalMousePosition = (l) globalMousePosition; \
    mouseButtons_ = (l) mouseButtons_;\
    }

#define MK_FROM_FINALIZER(cxt, ty, l) instance (FromFinalizer m cxt) => FromFinalizer (ty) where { \
    fromFinalizer = (l) . fromFinalizer }

#define MK_FREE_GAME(cxt, ty, l) instance (FreeGame m cxt) => FreeGame (ty) where { \
    draw = (l) . draw; \
    preloadBitmap = (l) . preloadBitmap; \
    takeScreenshot = (l) takeScreenshot; \
    bracket m = (l) (bracket m); \
    setFPS a = (l) (setFPS a); \
    setTitle t = (l) (setTitle t); \
    showCursor = (l) showCursor; \
    hideCursor = (l) hideCursor; \
    clearColor c = (l) (clearColor c); \
    getFPS = (l) getFPS; \
    }

hoistF :: (Functor f, Functor g) => (forall x. f x -> g x) -> Church.F f a -> Church.F g a
hoistF t = Church.iterM (wrap . t)
{-# INLINE hoistF #-}

MK_AFFINE(_COMMA_ Functor m, F m, hoistF)
MK_AFFINE(_COMMA_ Functor m, Free.Free m, Free.hoistFree)
MK_AFFINE(_COMMA_ Monad m, IterT m, hoistIterT)
MK_AFFINE(_COMMA_ Monad m, ReaderT r m, mapReaderT)
MK_AFFINE(_COMMA_ Monad m, Lazy.StateT s m, Lazy.mapStateT)
MK_AFFINE(_COMMA_ Monad m, Strict.StateT s m, Strict.mapStateT)
MK_AFFINE(_COMMA_ Monad m _COMMA_ Monoid w, Lazy.WriterT w m, Lazy.mapWriterT)
MK_AFFINE(_COMMA_ Monad m _COMMA_ Monoid w, Strict.WriterT w m, Strict.mapWriterT)
MK_AFFINE(_COMMA_ Monad m _COMMA_ Monoid w, Lazy.RWST r w s m, Lazy.mapRWST)
MK_AFFINE(_COMMA_ Monad m _COMMA_ Monoid w, Strict.RWST r w s m, Strict.mapRWST)
MK_AFFINE(_COMMA_ Monad m, IdentityT m, mapIdentityT)
MK_AFFINE(_COMMA_ Monad m, MaybeT m, mapMaybeT)
MK_AFFINE(_COMMA_ Monad m, ListT m, mapListT)
MK_AFFINE(_COMMA_ Monad m _COMMA_ Error e, ErrorT e m, mapErrorT)
MK_AFFINE(_COMMA_ Monad m, ContT r m, mapContT)

MK_PICTURE_2D(_COMMA_ Functor m, F m, Church.liftF, hoistF)
MK_PICTURE_2D(_COMMA_ Functor m, Free.Free m, Free.liftF, Free.hoistFree)
MK_PICTURE_2D(_COMMA_ Monad m, IterT m, lift, hoistIterT)
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

MK_LOCAL(_COMMA_ Functor m, F m, Church.liftF)
MK_LOCAL(_COMMA_ Functor m, Free.Free m, Free.liftF)
MK_LOCAL(_COMMA_ Monad m, IterT m, lift)
MK_LOCAL(_COMMA_ Monad m, ReaderT s m, lift)
MK_LOCAL(_COMMA_ Monad m, Lazy.StateT s m, lift)
MK_LOCAL(_COMMA_ Monad m, Strict.StateT s m, lift)
MK_LOCAL(_COMMA_ Monad m _COMMA_ Monoid w, Lazy.WriterT w m, lift)
MK_LOCAL(_COMMA_ Monad m _COMMA_ Monoid w, Strict.WriterT w m, lift)
MK_LOCAL(_COMMA_ Monad m _COMMA_ Monoid w, Lazy.RWST r w s m, lift)
MK_LOCAL(_COMMA_ Monad m _COMMA_ Monoid w, Strict.RWST r w s m, lift)
MK_LOCAL(_COMMA_ Monad m, IdentityT m, lift)
MK_LOCAL(_COMMA_ Monad m, MaybeT m, lift)
MK_LOCAL(_COMMA_ Monad m, ListT m, lift)
MK_LOCAL(_COMMA_ Monad m _COMMA_ Error e, ErrorT e m, lift)
MK_LOCAL(_COMMA_ Monad m, ContT r m, lift)

MK_KEYBOARD(_COMMA_ Functor m, F m, Church.liftF)
MK_KEYBOARD(_COMMA_ Functor m, Free.Free m, Free.liftF)
MK_KEYBOARD(_COMMA_ Monad m, IterT m, lift)
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

MK_MOUSE(_COMMA_ Functor m, F m, liftF)
MK_MOUSE(_COMMA_ Functor m, Free.Free m, Free.liftF)
MK_MOUSE(_COMMA_ Monad m, IterT m, lift)
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

MK_FROM_FINALIZER(_COMMA_ Functor m, F m, liftF)
MK_FROM_FINALIZER(_COMMA_ Functor m, Free.Free m, Free.liftF)
MK_FROM_FINALIZER(_COMMA_ Monad m, IterT m, lift)
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

MK_FREE_GAME(_COMMA_ Functor m, F m, liftF)
MK_FREE_GAME(_COMMA_ Functor m, Free.Free m, Free.liftF)
MK_FREE_GAME(_COMMA_ Monad m, IterT m, lift)
MK_FREE_GAME(_COMMA_ Monad m, Lazy.StateT s m, lift)
MK_FREE_GAME(_COMMA_ Monad m, Strict.StateT s m, lift)
MK_FREE_GAME(_COMMA_ Monad m _COMMA_ Monoid w, Lazy.WriterT w m, lift)
MK_FREE_GAME(_COMMA_ Monad m _COMMA_ Monoid w, Strict.WriterT w m, lift)
MK_FREE_GAME(_COMMA_ Monad m _COMMA_ Monoid w, Lazy.RWST r w s m, lift)
MK_FREE_GAME(_COMMA_ Monad m _COMMA_ Monoid w, Strict.RWST r w s m, lift)
MK_FREE_GAME(_COMMA_ Monad m, IdentityT m, lift)
MK_FREE_GAME(_COMMA_ Monad m, MaybeT m, lift)
MK_FREE_GAME(_COMMA_ Monad m, ListT m, lift)
MK_FREE_GAME(_COMMA_ Monad m _COMMA_ Error e, ErrorT e m, lift)
MK_FREE_GAME(_COMMA_ Monad m, ContT r m, lift)