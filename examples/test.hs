{-# LANGUAGE OverloadedStrings #-}
import FreeGame
import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict

figureTest :: Frame ()
figureTest = draw $ do
    color cyan -- 'colored' gives a color to the action.
        $ line [V2 80 80, V2 160 160] -- 'line' draws line through the given coordinates.

    color green $ polygon [V2 20 0, V2 100 20, V2 90 60, V2 30 70]

    color blue
        $ translate (V2 0 200) -- 'translate' moves the action.
        $ rotateD 45 -- 'rotateD' rotates the action. (in degrees).
        $ scale 2 -- 'scale' resizes the action. You can also specify as (V2 x y).
        $ polygonOutline [V2 20 0, V2 100 20, V2 50 60]

    color magenta
        $ thickness 3 -- 'thickness' sets the thickness of the lines.
        $ translate (V2 100 300)
        $ circleOutline 50

fontTest :: Font -> Frame ()
fontTest font = do
    translate (V2 100 300) $ color black
        $ text font 48 "Hello, World" -- Use 'text font size string' to draw a string.

bitmapTest :: Bitmap -> Frame ()
bitmapTest bmp = do
    color (Color 1 1 1 0.5) $ do
        translate (V2 300 350) $ bitmap bmp -- 'bitmap' creates an action from the bitmap.
        translate (V2 300 360) $ bitmap bmp
    translate (V2 100 350) $ bitmap (cropBitmap bmp (32, 32) (0, 0)) -- You can slice bitmaps using 'cropBitmap'.

mouseTest :: Frame ()
mouseTest = do
    p <- mousePosition
    l <- mouseButtonL
    r <- mouseButtonR
    let col = case (l, r) of
            (False, False) -> black
            (True, False) -> red
            (False, True) -> blue
            (True, True) -> blend 0.5 red blue
    translate p $ color col $ thickness 4 $ circleOutline 16

main = runGame $ do
    bmp <- embedIO $ readBitmap "logo.png"
    bmp' <- embedIO $ readBitmap "Icon.png"
    font <- embedIO $ loadFont "VL-PGothic-Regular.ttf"
    foreverFrame $ do
        
        bitmapTest bmp'
        
        figureTest

        fontTest font

        translate (V2 240 240) $ do
            mouseTest

        whenM (keyDown KeyS) $ takeScreenshot >>= writeBitmap "capture.png"