{-# LANGUAGE BangPatterns, DeriveFunctor, Rank2Types #-}
module FreeGame.Picture where
import FreeGame.Types
import FreeGame.Data.Bitmap
import Data.Color
import Linear
import Unsafe.Coerce
import Control.Applicative

infixr 5 `translate`
infixr 5 `rotateR`
infixr 5 `rotateD`
infixr 5 `scale`
infixr 5 `color`
infixr 5 `thickness`
infixr 5 `blendMode`

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
    -- | Same as 'bitmap', but it does not create a cache.
    bitmapOnce :: Bitmap -> p ()
    line :: [Vec2] -> p ()
    polygon :: [Vec2] -> p ()
    polygonOutline :: [Vec2] -> p ()
    circle :: Double -> p ()
    circleOutline :: Double -> p ()
    thickness :: Float -> p a -> p a
    color :: Color -> p a -> p a
    blendMode :: BlendMode -> p a -> p a

class Affine p => Local p where
    getLocation :: p (Location a)

data Location a = Location (Vec2 -> Vec2) (Vec2 -> Vec2) deriving Functor

coerceLocation :: Location a -> Location b
coerceLocation = unsafeCoerce

flipLocation :: Location a -> Location b
flipLocation (Location f g) = Location g f

localize :: Local f => Vec2 -> f Vec2
localize v = fmap (\(Location _ g) -> g v) getLocation

globalize :: Local f => Vec2 -> f Vec2
globalize v = fmap (\(Location f _) -> f v) getLocation

instance Affine Location where
    translate v (Location f g) = Location (f . (^+^v)) ((^-^v) . g)
    rotateR t (Location f g) = Location (f . rot2 t) (rot2 (-t) . g)
    scale v (Location f g) = Location (f . (*v)) ((/v) . g)

rot2 :: Floating a => a -> V2 a -> V2 a
rot2 t (V2 x y) = V2 (p * x + q * y) (-q * x + p * y) where
    !p = cos t
    !q = sin t

newtype Picture a = Picture { runPicture :: forall m. (Applicative m, Monad m, Picture2D m, Local m) => m a }
