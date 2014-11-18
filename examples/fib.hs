{-# LANGUAGE TemplateHaskell #-}
import FreeGame
import Control.Lens
import Control.Monad.State
import Control.Monad.Trans.Maybe

data Mode = Scroll Double | Dist Double

data World = World
    { _seq0 :: [Int]
    , _seq1 :: [Int]
    , _offset :: Vec2
    , _target :: Int
    , _mode :: Mode
    }
makeLenses ''World

fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

speed = 1

update :: Font -> StateT World Game ()
update font = do
    s0 <- use seq0
    s1 <- use seq1
    ofs <- use offset
    t <- use target
    
    let v = s0 !! t + s1 !! t
    color black $ do
      translate (V2 24 240) $ text font 24 "fibs"
      translate (V2 24 280) $ text font 24 "tail fibs"
      forM_ (zip [0..] s0) $ \(i, v) -> translate (ofs + V2 (i * 36) 240) $ text font 24 (show v)
      forM_ (zip [0..] s1) $ \(i, v) -> translate (ofs + V2 (i * 36) 280) $ text font 24 (show v)
    color blue $ line [V2 400 480, V2 400 0]
    ph <- use mode
    case ph of
      Scroll ph
        | ph > 0 -> do
          mode .= Scroll (ph - 1)
          
          color black $ translate (V2 390 320) $ text font 24 (show v)
          offset .= ofs - V2 speed 0
        | otherwise -> mode .= Dist 0
      Dist ph
        | ph >= 1 -> do
          seq0 .= s0 ++ [v]
          seq1 .= s1 ++ [v]
          mode .= Scroll 36
          target += 1
        | otherwise -> color black $ do
          translate (p0 ^* (1 - ph) + V2 (390+36) 240 ^* ph) $ text font 24 (show v)
          translate (p0 ^* (1 - ph) + V2 (390) 280 ^* ph) $ text font 24 (show v)
          mode .= Dist (ph + 1/30)
  where
    p0 = V2 390 320

mainLoop :: Font -> World -> Game ()
mainLoop font s = do
  s' <- execStateT (update font) s
  tick
  mainLoop font s'

main = runGameDefault $ do
    font <- loadFont "VL-PGothic-Regular.ttf"
    runMaybeT $ forever $ do {tick;()<-whenM (keyDown KeySpace) mzero; return ()}
    mainLoop font $ World [1,1] [1] (V2 400 0) 0 (Scroll 36)