{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE CPP #-}
import Linear
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

type Vec2 = Vec2

hoistFreeR :: (Functor f, MonadFree g m) => (f (m a) -> g (m a)) -> Free.Free f a -> m a
hoistFreeR _ (Free.Pure a) = return a
hoistFreeR t (Free.Free f) = wrap . t $ fmap (hoistFreeR t) f
{-# INLINE[~4] hoistFreeR #-}

hoistFR :: MonadFree g m => (f (m a) -> g (m a)) -> F f a -> m a
hoistFR t (F m) = m return (wrap . t)
{-# INLINE[~4] hoistFR #-}

{-# RULES
"hoistFreeR/hoistFreeR"[5]   forall f g m. hoistFreeR f (hoistFreeR g m) = hoistFreeR (f . g) m
"hoistFR/hoistFR"[5]     forall f g m. hoistFR f (hoistFR g m) = hoistFR (f . g) m
 #-}

-- | The class of types that can be regarded as a kind of picture.
class Picture2D p where
    -- | Construct a 'Picture2D' from a 'Bitmap'.
    fromBitmap :: Bitmap -> p ()
    -- | (radians)
    rotateR :: Double -> p a -> p a
    -- | (degrees)
    rotateD :: Double -> p a -> p a
    scale :: Vec2 -> p a -> p a
    translate :: Vec2 -> p a -> p a
    colored :: Color -> p a -> p a

    rotateR = rotateD . (* 180) . (/ pi)
    rotateD = rotateR . (/ 180) . (* pi)

-- | Deprecated synonym for 'rotateD'.
rotate :: Picture2D p => Double -> p a -> p a
rotate = rotateD

{-# DEPRECATED rotate "Use rotateD instead" #-} 

class Picture2D p => Figure2D p where
    line :: [Vec2] -> p ()
    polygon :: [Vec2] -> p ()
    polygonOutline :: [Vec2] -> p ()
    circle :: Double -> p ()
    circleOutline :: Double -> p ()
    thickness :: Float -> p a -> p a

class Sound p where
    fromWave :: Wave -> p ()
    volume :: Float -> p a -> p a
    pan :: Float -> p a -> p a

-- | The class of types that can handle inputs of the keyboard.
class Keyboard t where
    keyState :: Key -> t Bool

{-# DEPRECATED keySpecial "Use keyState instead" #-}
keySpecial :: Keyboard t => SpecialKey -> t Bool
keySpecial = keyState

{-# DEPRECATED keySpecial "Use keyChar instead" #-}
keyChar :: Keyboard t => Char -> t Bool
keyChar = undefined

-- | The class of types that can handle inputs of the mouse.
class Mouse t where
    mousePosition :: t (Vec2)
    mouseWheel :: t Int
    mouseButtonL :: t Bool
    mouseButtonM :: t Bool
    mouseButtonR :: t Bool

class FromFinalizer m where
    fromFinalizer :: FinalizerT IO a -> m a

instance FromFinalizer (FinalizerT IO) where
    fromFinalizer = id

#define _COMMA_ ,

#define MK_PICTURE_2D(cxt, ty, l, t) instance (Picture2D m cxt) => Picture2D (ty) where { \
    fromBitmap = (l) . fromBitmap; \
    rotateD = (t) . rotateD; \
    rotateR = (t) . rotateR; \
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
    keyPressed = (l) . keyPressed }

#define MK_MOUSE(cxt, ty, l) instance (Mouse m cxt) => Mouse (ty) where { \
    mousePosition = (l) mousePosition; \
    mouseWheel = (l) mouseWheel; \
    mouseButtonL = (l) mouseButtonL; \
    mouseButtonR = (l) mouseButtonR; \
    mouseButtonM = (l) mouseButtonM }

#define MK_FROM_FINALIZER(cxt, ty, l) instance (FromFinalizer m cxt) => FromFinalizer (ty) where { \
    fromFinalizer = (l) . fromFinalizer }