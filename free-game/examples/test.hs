{-# LANGUAGE TemplateHaskell #-}
import Graphics.UI.FreeGame
import Control.Applicative
import Control.Monad
import Linear
import Control.Monad.Free
import Control.Monad.State
import Data.Void
import Control.Lens -- using lens (http://hackage.haskell.org/package/lens)

$(loadBitmaps "images")

data Object = Object
    { _position :: V2 Float
    , _velocity :: V2 Float
    , _pressed :: Bool
    }

$(makeLenses ''Object)

obj :: StateT Object (Free GUI) Void
obj = forever $ do
    pos@(V2 x y) <- use position

    vel@(V2 dx dy) <- use velocity

    let dx' | x <= 0 = abs dx
            | x >= 640 = -(abs dx)
            | otherwise = dx
        dy' | y <= 0 = abs dy
            | y >= 480 = -(abs dy)
            | otherwise = dy

    position .= pos + vel
    velocity .= V2 dx' dy'

    mpos <- input mousePosition

    w <- if norm (mpos - pos) < 32
        then do
            btn <- use pressed
            btn' <- input mouseButtonL
            when (not btn && btn') $ velocity <~ (^*4) <$> sinCos <$> randomness (0, 2 * pi)
            pressed .= btn'
            return id

        else return $ Colored (transparent 0.7 white)

    draw $ Translate pos $ w (Bitmap _logo_png)

    tick

initial :: Free GUI Void
initial = do
    x <- randomness (0,640)
    y <- randomness (0,480)
    a <- randomness (0, 2 * pi)
    evalStateT obj $ Object (V2 x y) (sinCos a ^* 4) False

main = runSimple def (replicate 100 initial) $ mapM untickInfinite
