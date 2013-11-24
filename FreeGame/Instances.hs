{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE CPP #-}
module Graphics.UI.FreeGame.Instances () where

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

#define _COMMA_ ,

#define MK_PICTURE_2D(cxt, ty, l, t) instance (Picture2D m cxt) => Picture2D (ty) where { \
    fromBitmap b = l (fromBitmap l); \
    {-# INLINE fromBitmap #-}; \
    rotateD a = t (rotateD a); \
    {-# INLINE rotateD #-}; \
    rotateR a = t (rotateR a); \
    {-# INLINE rotateR #-}; \
    translate a = t (translate a); \
    scale a = t (scale a); \
    colored a = t (colored a) }

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
    keyPressed = (l) . keyPressed }

#define MK_MOUSE(cxt, ty, l) instance (Mouse m cxt) => Mouse (ty) where { \
    mousePosition = (l) mousePosition; \
    mouseWheel = (l) mouseWheel; \
    mouseButtonL = (l) mouseButtonL; \
    mouseButtonR = (l) mouseButtonR; \
    mouseButtonM = (l) mouseButtonM }

#define MK_FROM_FINALIZER(cxt, ty, l) instance (FromFinalizer m cxt) => FromFinalizer (ty) where { \
    fromFinalizer = (l) . fromFinalizer }