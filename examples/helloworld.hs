import Graphics.UI.FreeGame
main = runGame def $ do
    font <- embedIO $ loadFont "VL-PGothic-Regular.ttf"
    foreverTick $ translate (V2 80 200) $ colored black $ text font 40 "Hello, World"