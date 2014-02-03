import FreeGame
import System.Random

blendTest :: Bitmap -> Frame ()
blendTest bmp = do
  blendMode Normal $ translate (V2 100 200) $ bitmap bmp

  blendMode Inverse $ translate (V2 200 200) $ bitmap bmp

  blendMode Add $ translate (V2 300 200) $ bitmap bmp
  
  blendMode Multiply $ translate (V2 100 400) $ bitmap bmp

  blendMode Screen $ translate (V2 200 400) $ bitmap bmp

main = runGame Windowed (BoundingBox 0 0 640 480) $ do
  hideCursor  
  bmp <- embedIO $ readBitmap "Icon.png"
  bmp' <- embedIO $ readBitmap "logo.png"

  foreverFrame $ do
    translate (V2 300 350) $ bitmap bmp
    blendTest bmp'