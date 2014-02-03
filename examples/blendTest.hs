import FreeGame

bmpTest :: Bitmap -> Frame ()
bmpTest bmp = do
  translate (V2 300 350) $ bitmap bmp

blendTest :: Bitmap -> Frame ()
blendTest bmp = do
  setBlendMode NormalNonAlpha
  translate (V2 140 200) $ bitmap bmp

  setBlendMode NormalAlpha
  translate (V2 240 200) $ bitmap bmp

  setBlendMode Inverse
  translate (V2 340 200) $ bitmap bmp

  setBlendMode AddNonAlpha
  translate (V2 140 300) $ bitmap bmp

  setBlendMode AddAlpha
  translate (V2 240 300) $ bitmap bmp

  setBlendMode Multiply
  translate (V2 340 300) $ bitmap bmp

  setBlendMode Screen
  translate (V2 140 400) $ bitmap bmp

  setBlendMode Remove
  translate (V2 240 400) $ bitmap bmp

  setBlendMode NormalAlpha

main = runGame Windowed (BoundingBox 0 0 640 480) $ do
    bmp <- embedIO $ readBitmap "Icon.png"
    bmp' <- embedIO $ readBitmap "logo.png"

    foreverFrame $ do
      bmpTest bmp
      blendTest bmp'
