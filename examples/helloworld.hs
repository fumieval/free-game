import FreeGame
import FreeGame.Core
import Debug.Trace

main = runGame Windowed (Box (V2 0 0) (V2 640 480)) $ do
  gen <- invoke $ sineGenerator 0
  connectAudio gen
  stand

instance Graphic Sound

instance HandleKeyboard Sound

instance HandleMouse Sound

instance Audio Sound where
  pullAudio t n = Sound t n id

data Sound a = Sound Time Int ([V2 Float] -> a)

sineGenerator k0 = Core
  $ \(Sound t n cont) -> return (cont [pure $ sin $ fromIntegral (k0 + i) / 44100 * 440 * 2 * pi | i <- [0..n-1]], sineGenerator $ k0 + n)