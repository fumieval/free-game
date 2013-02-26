{-# LANGUAGE ImplicitParams, TemplateHaskell #-}
import Graphics.FreeGame.Simple
import Control.Applicative
import Control.Monad
import Data.Vect
import Control.Monad.State

import Control.Lens -- using lens (http://hackage.haskell.org/package/lens)

data Object = Object
    { _position :: Vec2
    , _velocity :: Vec2
    , _pressed :: Bool
    }

$(makeLenses ''Object)

obj :: (?pic :: Picture) => StateT Object Game ()
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

    if norm (mpos &- pos) < 32
        then do
            drawPicture $ Translate pos ?pic
            btn <- use pressed
            btn' <- getButtonState MouseLeft
            when (not btn && btn') $ velocity <~ (&*4) <$> sinCos <$> randomness (0, 2 * pi)
            pressed .= btn'

        else drawPicture $ Translate pos $ Colored (transparent 0.7 white) ?pic

    tick

initial :: (?pic :: Picture) => Game ()
initial = do
    x <- randomness (0,640)
    y <- randomness (0,480)
    a <- randomness (0, 2 * pi)
    evalStateT obj $ Object (Vec2 x y) (sinCos a &* 4) False

main = do
    bmp <- loadBitmapFromFile "logo.png"
    let ?pic = BitmapPicture bmp

    runSimple defaultGameParam (replicate 100 initial) $ mapM untickGame
