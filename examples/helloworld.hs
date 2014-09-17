{-# LANGUAGE Rank2Types #-}
import FreeGame
import FreeGame.Class
import Debug.Trace
import FreeGame.Component
import FreeGame.Component.Deck

meter deck = oneshot $ \(PullGraphic _ cont) -> do
  let s x = (10 + max (log x / log 10) (-10)) / 10
  V2 a b <- fmap (fmap s) $ deck .- RMS 1024
  cont $ translate (V2 0 240) $ do
    color black $ do
      polygonOutline [V2 0 0, V2 40 0, V2 40 (-240), V2 0 (-240)]
    color red $ do
      polygonOutline [V2 0 0, V2 16 0, V2 16 (-a * 240), V2 0 (-a * 240)]
      polygonOutline [V2 24 0, V2 40 0, V2 40 (-b * 240), V2 24 (-b * 240)]

main = runGame Windowed (Box (V2 0 0) (V2 40 480)) $ do
  deck <- invoke emptyDeck
  connectAudio deck
  invoke (meter deck) >>= connectGraphic
  src <- liftIO $ readSound "Monoidal Purity.wav"
  deck .- Load src
  deck .- Play
  stand

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
    scale v (Locationf  g) = Location (f . (*v)) ((/v) . g)

rot2 :: Floating a => a -> V2 a -> V2 a
rot2 t (V2 x y) = V2 (p * x + q * y) (-q * x + p * y) where
    !p = cos t
    !q = sin t