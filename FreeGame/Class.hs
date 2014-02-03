{-# LANGUAGE BangPatterns, Rank2Types, DeriveFunctor #-}
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
import FreeGame.Internal.Finalizer
import Data.Color
import Control.Monad.IO.Class
import qualified Data.Map as Map

infixr 5 `translate`
infixr 5 `rotateR`
infixr 5 `rotateD`
infixr 5 `scale`
infixr 5 `color`
infixr 5 `colored`

class Functor p => Affine p where
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
    color :: Color -> p a -> p a
    blendMode :: BlendMode -> p a -> p a

{-# DEPRECATED fromBitmap "Use bitmap instead" #-}
fromBitmap :: Picture2D p => Bitmap -> p ()
fromBitmap = bitmap

{-# DEPRECATED colored "Use color instead" #-}
colored :: Picture2D p => Color -> p a -> p a
colored = color

class Affine p => Local p where
    getLocation :: p (Location a)

data Location a = Location (Vec2 -> Vec2) (Vec2 -> Vec2) deriving Functor

coerceLocation :: Location a -> Location b
coerceLocation = unsafeCoerce

flipLocation :: Location a -> Location b
flipLocation (Location f g) = Location g f

localize :: Local f => Vec2 -> f Vec2
localize v = (\(Location _ g) -> g v) <$> getLocation

globalize :: Local f => Vec2 -> f Vec2
globalize v = (\(Location f _) -> f v) <$> getLocation

instance Affine Location where
    translate v (Location f g) = Location (f . (^+^v)) ((^-^v) . g)
    rotateR t (Location f g) = Location (f . rot2 t) (rot2 (-t) . g)
    scale v (Location f g) = Location (f . (*v)) ((/v) . g)

rot2 :: Floating a => a -> V2 a -> V2 a
rot2 t (V2 x y) = V2 (p * x + q * y) (-q * x + p * y) where
    !p = cos t
    !q = sin t

data ButtonState = Down | Press | Up | Release　| ChatterDown | ChatterUp

buttonDown :: ButtonState -> ButtonState
buttonDown Release = Down
buttonDown Up = ChatterDown
buttonDown ChatterUp = ChatterDown
buttonDown x = x

buttonUp :: ButtonState -> ButtonState
buttonUp Press = Up
buttonUp Down = ChatterUp
buttonUp ChatterDown = ChatterUp
buttonUp x = x

buttonStay :: ButtonState -> ButtonState
buttonStay Down = Press
buttonStay Press = Press
buttonStay Up = Release
buttonStay Release = Release
buttonStay ChatterDown = Press
buttonStay ChatterUp = Release

isDown :: ButtonState -> Bool
isDown Down = True
isDown ChatterDown = True
isDown ChatterUp = True
isDown _ = False

isUp :: ButtonState -> Bool
isUp Up = True
isUp ChatterUp = True
isUp ChatterDown = True
isUp _ = False

isPressed :: ButtonState -> Bool
isPressed Down = True
isPressed ChatterUp = True
isPressed Press = True
isPressed ChatterDown = True
isPressed _ = False

class Functor f => Keyboard f where
    keyStates_ :: f (Map.Map Key ButtonState)

keyStates :: Keyboard f => f (Map.Map Key Bool)
keyStates = Map.map isPressed <$> keyStates_

keyPress :: Keyboard f => Key -> f Bool
keyPress k = isPressed <$> (Map.! k) <$> keyStates_

keyDown :: Keyboard f => Key -> f Bool
keyDown k = isDown <$> (Map.! k) <$> keyStates_

keyUp :: Keyboard f => Key -> f Bool
keyUp k = isUp <$> (Map.! k) <$> keyStates_

class Functor f => Mouse f where
    globalMousePosition :: f Vec2
    mouseButtons_ :: f (Map.Map Int ButtonState)

-- | Returns the relative coordinate of the cursor.
mousePosition :: (Applicative f, Mouse f, Local f) => f Vec2
mousePosition = (\v (Location _ g) -> g v) <$> globalMousePosition <*> getLocation

mouseButtons :: Mouse f => f (Map.Map Int Bool)
mouseButtons = Map.map isPressed <$> mouseButtons_

mouseButton :: Mouse f => Int -> f Bool
mouseButton k = isPressed <$> (Map.! k) <$> mouseButtons_

mouseDown :: Mouse f => Int -> f Bool
mouseDown k = isDown <$> (Map.! k) <$> mouseButtons_

mouseUp :: Mouse f => Int -> f Bool
mouseUp k = isUp <$> (Map.! k) <$> mouseButtons_

mouseButtonL :: Mouse f => f Bool
mouseButtonL = mouseButton 0

mouseButtonR :: Mouse f => f Bool
mouseButtonR = mouseButton 1

mouseButtonM :: Mouse f => f Bool
mouseButtonM = mouseButton 2

mouseDownL :: Mouse f => f Bool
mouseDownL = mouseDown 0

mouseDownR :: Mouse f => f Bool
mouseDownR = mouseDown 1

mouseDownM :: Mouse f => f Bool
mouseDownM = mouseDown 2

mouseUpL :: Mouse f => f Bool
mouseUpL = mouseUp 0

mouseUpR :: Mouse f => f Bool
mouseUpR = mouseUp 1

mouseUpM :: Mouse f => f Bool
mouseUpM = mouseUp 2

class FromFinalizer m where
    fromFinalizer :: FinalizerT IO a -> m a

instance FromFinalizer (FinalizerT IO) where
    fromFinalizer = id

embedIO :: FromFinalizer m => IO a -> m a
embedIO m = fromFinalizer (liftIO m)
{-# INLINE embedIO #-}
