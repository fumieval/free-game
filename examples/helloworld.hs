import FreeGame

main = runGame Windowed (BoundingBox 0 0 640 480) $ do
    hideCursor
    font <- embedIO $ loadFont "VL-PGothic-Regular.ttf"
    foreverFrame $ translate (V2 80 200) $ color black $ text font 40 "Hello, World"
