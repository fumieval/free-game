{-# LANGUAGE BangPatterns, FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  FreeGame.Class
-- Copyright   :  (C) 2013 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
-- 
----------------------------------------------------------------------------
module FreeGame.Class where

import Linear

import Control.Applicative
import Unsafe.Coerce
import FreeGame.Types
import FreeGame.Data.Bitmap
import FreeGame.Data.Wave
import FreeGame.Internal.Finalizer
import Data.Color
import Control.Monad.IO.Class
import qualified Data.Map as Map

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
    getLocation :: p (Location a)

data Location a = Location (Vec2 -> Vec2) (Vec2 -> Vec2)

coerceLocation :: Location a -> Location b
coerceLocation = unsafeCoerce

flipLocation :: Location a -> Location b
flipLocation (Location f g) = Location g f

instance Affine Location where
    translate v (Location f g) = Location ((^+^v) . f) (g . (^-^v))
    rotateR t (Location f g) = Location (rot2 t . f) (g . rot2 (-t))
    scale v (Location f g) = Location ((*v) . f) (g . (/v))

rot2 :: Floating a => a -> V2 a -> V2 a
rot2 a (V2 !x !y) = V2 (p * x + q * y) (-q * x + p * y) where
    !d = a * (pi / 180) 
    !p = cos d
    !q = sin d

class Keyboard t where
    keyStates :: t (Map.Map Key Bool)
    previousKeyStates :: t (Map.Map Key Bool)

keyPress :: (Functor f, Keyboard f) => Key -> f Bool
keyPress k = (Map.! k) <$> keyStates

keyDown :: (Applicative f, Keyboard f) => Key -> f Bool
keyDown k = go <$> keyStates <*> previousKeyStates where
    go m n = m Map.! k && not (n Map.! k)

keyUp :: (Applicative f, Keyboard f) => Key -> f Bool
keyUp k = go <$> keyStates <*> previousKeyStates where
    go m n = not (m Map.! k) && n Map.! k

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
    mouseButtons :: t (Map.Map Int Bool)
    previousMouseButtons :: t (Map.Map Int Bool)

-- | Returns the relative coordinate of the cursor.
mousePosition :: (Applicative f, Mouse f, Local f) => f Vec2
mousePosition = (\v (Location f _) -> f v) <$> globalMousePosition <*> getLocation

mouseButton :: (Functor f, Mouse f) => Int -> f Bool
mouseButton k = (Map.! k) <$> mouseButtons

mouseDown :: (Applicative f, Mouse f) => Int -> f Bool
mouseDown k = go <$> mouseButtons <*> previousMouseButtons where
    go m n = m Map.! k && not (n Map.! k)

mouseUp :: (Applicative f, Mouse f) => Int -> f Bool
mouseUp k = go <$> mouseButtons <*> previousMouseButtons where
    go m n = not (m Map.! k) && n Map.! k

mouseButtonL :: (Functor f, Mouse f) => f Bool
mouseButtonL = mouseButton 0

mouseButtonR :: (Functor f, Mouse f) => f Bool
mouseButtonR = mouseButton 1

mouseButtonM :: (Functor f, Mouse f) => f Bool
mouseButtonM = mouseButton 2

mouseDownL :: (Applicative f, Mouse f) => f Bool
mouseDownL = mouseDown 0

mouseDownR :: (Applicative f, Mouse f) => f Bool
mouseDownR = mouseDown 1

mouseDownM :: (Applicative f, Mouse f) => f Bool
mouseDownM = mouseDown 2

mouseUpL :: (Applicative f, Mouse f) => f Bool
mouseUpL = mouseUp 0

mouseUpR :: (Applicative f, Mouse f) => f Bool
mouseUpR = mouseUp 1

mouseUpM :: (Applicative f, Mouse f) => f Bool
mouseUpM = mouseUp 2

class FromFinalizer m where
    fromFinalizer :: FinalizerT IO a -> m a

instance FromFinalizer (FinalizerT IO) where
    fromFinalizer = id

embedIO :: FromFinalizer m => IO a -> m a
embedIO m = fromFinalizer (liftIO m)
{-# INLINE embedIO #-}