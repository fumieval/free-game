import FreeGame

main = runGame Windowed (Box (V2 0 0) (V2 640 480)) $ do
    hideCursor
    font <- loadFont "VL-PGothic-Regular.ttf"
    foreverFrame $ do
        V2 x y <- mousePosition
        perspective 1 60 $ viewFromToUp (V3 240 240 (-5)) (V3 240 240 0) (V3 0 1 0)
            $ translate3 (V3 240 240 (-3)) $ color black $ text font 60 "Hello, world!"
        translate (V2 240 240) $ text font 60 "Hello, world!"