module Graphics.UI.FreeGame.Class where

import Linear
import Control.Monad.Free.Class
import Control.Monad.Free.Church
import qualified Control.Monad.Free as Free

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

{-# DEPRECATED keySpecial "Use keyState instead" #-}
keySpecial :: Keyboard t => SpecialKey -> t Bool
keySpecial = keyState

{-# DEPRECATED keyChar "Use keyState instead" #-}
keyChar :: Keyboard t => Char -> t Bool
keyChar = undefined

{-# DEPRECATED fromBitmap "Use bitmap instead" #-}
fromBitmap = bitmap

class Mouse t where
    globalMousePosition :: t Vec2
    mouseWheel :: t Int
    mouseButtonL :: t Bool
    mouseButtonM :: t Bool
    mouseButtonR :: t Bool

class FromFinalizer m where
    fromFinalizer :: FinalizerT IO a -> m a

instance FromFinalizer (FinalizerT IO) where
    fromFinalizer = id