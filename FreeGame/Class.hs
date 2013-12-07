{-# LANGUAGE BangPatterns, FlexibleInstances #-}
module FreeGame.Class where

import Linear

import Control.Applicative
import Control.Monad.Free.Class
import Control.Monad.Free.Church
import qualified Control.Monad.Free as Free
import Unsafe.Coerce
import FreeGame.Types
import FreeGame.Data.Bitmap
import FreeGame.Data.Wave
import FreeGame.Internal.Finalizer
import Data.Color
import Control.Monad.IO.Class

class FromFile a where
    fromFile :: MonadIO m => FilePath -> m a

class Affine p where
    -- | (radians)
    rotateR :: Double -> p a -> p a
    -- | (degrees)
    rotateD :: Double -> p a -> p a
    scale :: Vec2 -> p a -> p a
    translate :: Vec2 -> p a -> p a

    rotateR = rotateD . (* 180) . (/ pi)
    rotateD = rotateR . (/ 180) . (* pi)

-- | The class of types that can be regarded as a kind of picture.
class Affine p => Picture2D p where
    -- | Construct a 'Picture2D' from a 'Bitmap'.
    bitmap :: Bitmap -> p ()
    line :: [Vec2] -> p ()
    polygon :: [Vec2] -> p ()
    polygonOutline :: [Vec2] -> p ()
    circle :: Double -> p ()
    circleOutline :: Double -> p ()
    thickness :: Float -> p a -> p a
    colored :: Color -> p a -> p a

class Affine p => Local p where
    getViewPort :: p (ViewPort a)

class Keyboard t where
    keyState :: Key -> t Bool

class Sound t where
    play :: Wave -> t ()

{-

{-# DEPRECATED keySpecial "Use keyState instead" #-}
keySpecial :: Keyboard t => SpecialKey -> t Bool
keySpecial = keyState

{-# DEPRECATED keyChar "Use keyState instead" #-}
keyChar :: Keyboard t => Char -> t Bool
keyChar = undefined

-}

{-# DEPRECATED fromBitmap "Use bitmap instead" #-}
fromBitmap :: Picture2D p => Bitmap -> p ()
fromBitmap = bitmap

class Mouse t where
    globalMousePosition :: t Vec2
    mouseWheel :: t Int
    mouseButtonL :: t Bool
    mouseButtonM :: t Bool
    mouseButtonR :: t Bool

mousePosition :: (Applicative f, Mouse f, Local f) => f Vec2
mousePosition = (\v (ViewPort f _) -> f v) <$> globalMousePosition <*> getViewPort

class FromFinalizer m where
    fromFinalizer :: FinalizerT IO a -> m a

instance FromFinalizer (FinalizerT IO) where
    fromFinalizer = id

data ViewPort a = ViewPort (Vec2 -> Vec2) (Vec2 -> Vec2)

coerceViewPort :: ViewPort a -> ViewPort b
coerceViewPort = unsafeCoerce

flipViewPort :: ViewPort a -> ViewPort b
flipViewPort (ViewPort f g) = ViewPort g f

instance Affine ViewPort where
    translate v (ViewPort f g) = ViewPort ((^+^v) . f) (g . (^-^v))
    rotateR t (ViewPort f g) = ViewPort (rot2 t . f) (g . rot2 (-t))
    scale v (ViewPort f g) = ViewPort ((*v) . f) (g . (/v))

rot2 :: Floating a => a -> V2 a -> V2 a
rot2 a (V2 !x !y) = V2 (p * x + q * y) (-q * x + p * y) where
    !d = a * (pi / 180) 
    !p = cos d
    !q = sin d