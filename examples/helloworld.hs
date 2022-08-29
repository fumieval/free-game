import FreeGame

main = runGame Windowed (Box (V2 0 0) (V2 640 480)) $ do
    hideCursor
    font <- loadFont "examples/VL-PGothic-Regular.ttf"
    foreverFrame $ translate (V2 80 200) $ color black $ text font 40 "Hello, World"
