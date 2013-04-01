{-# LANGUAGE TemplateHaskell #-}
import Graphics.FreeGame.Simple
import Control.Applicative
import Control.Monad
import Data.Vect
import Control.Monad.State
import Data.Void
import Control.Lens -- using lens (http://hackage.haskell.org/package/lens)

$(loadBitmaps "images")

data Object = Object
    { _position :: Vec2
    , _velocity :: Vec2
    , _pressed :: Bool
    }

$(makeLenses ''Object)

obj :: StateT Object Game Void
obj = forever $ do
    pos@(Vec2 x y) <- use position

    vel@(Vec2 dx dy) <- use velocity

    let dx' | x <= 0 = abs dx
            | x >= 640 = -(abs dx)
            | otherwise = dx
        dy' | y <= 0 = abs dy
            | y >= 480 = -(abs dy)
            | otherwise = dy

    position .= pos &+ vel
    velocity .= Vec2 dx' dy'

    mpos <- getMousePosition

    w <- if norm (mpos &- pos) < 32
        then do
            btn <- use pressed
            btn' <- getButtonState MouseLeft
            when (not btn && btn') $ velocity <~ (&*4) <$> sinCos <$> randomness (0, 2 * pi)
            pressed .= btn'
            return id

        else return $ Colored (transparent 0.7 white)

    drawPicture $ Translate pos $ w (Bitmap _logo_png)

    tick

initial :: Game Void
initial = do
    x <- randomness (0,640)
    y <- randomness (0,480)
    a <- randomness (0, 2 * pi)
    evalStateT obj $ Object (Vec2 x y) (sinCos a &* 4) False

main = runSimple defaultGameParam (replicate 100 initial) $ mapM untickInfinite
