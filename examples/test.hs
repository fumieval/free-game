{-# LANGUAGE OverloadedStrings #-}
import Graphics.UI.FreeGame
import Control.Applicative
import Control.Monad

figureTest :: Game ()
figureTest = do
    colored cyan $ line [V2 80 80, V2 160 160]
    colored green $ polygon [V2 20 0, V2 100 20, V2 90 60, V2 30 70]
    colored blue $ translate (V2 0 200) $ rotate 45 $ scale 2 $ polygonOutline [V2 20 0, V2 100 20, V2 50 60]
    colored red $ translate (V2 200 300) $ circle 40
    colored magenta $ thickness 3 $ translate (V2 100 300) $ circleOutline 50

fontTest :: Font -> Game ()
fontTest font = do
    () <- translate (V2 100 300) $ colored black
        $ runTextT Nothing font 17 "Hello, World"


main = runGame def $ do
    font <- embedIO (loadFont "VL-PGothic-Regular.ttf")
    bmp <- embedIO (loadBitmapFromFile "logo.png")
    forever $ do
        translate (V2 300 350) $ fromBitmap bmp
        figureTest
        fontTest font
        tick