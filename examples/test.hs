{-# LANGUAGE ImplicitParams #-}
import Graphics.FreeGame
import Control.Applicative
import Control.Monad
import Data.Vect

act :: (?pic :: Picture) => Vec2 -> Vec2 -> Float -> Bool -> Game ()
act pos@(Vec2 x y) vel@(Vec2 dx dy) angle btn = do

    drawPicture $ Translate pos $ Rotate angle ?pic
    
    let dx' | x <= 0 = abs dx
            | x >= 640 = -(abs dx)
            | otherwise = dx
        dy' | y <= 0 = abs dy
            | y >= 480 = -(abs dy)
            | otherwise = dy

    mouse <- getMouseState
    vel' <- if not btn && leftButton mouse && norm (mousePosition mouse &- pos) < 32
        then (&*3) <$> sinCos <$> randomness (0, 2 * pi)
        else return (Vec2 dx' dy')
    tick
    act (pos &+ vel) vel' (angle + 1) (leftButton mouse)

initial :: (?pic :: Picture) => Game ()
initial = do
    x <- randomness (0,640)
    y <- randomness (0,480)
    a <- randomness (0, 2 * pi)
    act (Vec2 x y) (sinCos a &* 4) 0 False

main = runGame defaultGameParam $ do
    pic <- loadPictureFromFile "logo.png"
    let ?pic = pic
    run (replicate 24 initial)
    where
        run ms = do
            ms' <- mapM untickGame ms
            tick
            run ms'