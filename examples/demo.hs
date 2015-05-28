-- Pointless demo
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
    translate (V2 30 300) $ color white $ do
        text font 48 "The quick brown fox"
        color red $ line [V2 (-100) 0, V2 640 0]
        translate (V2 0 80) $ do
            text font 48 "jumps over the lazy dog"
            color red $ line [V2 (-100) 0, V2 640 0]
        translate (V2 0 160) $ do
            text font 48 "0123456789" -- Use 'text font size string' to draw a string.
            color red $ line [V2 (-100) 0, V2 640 0]
        
        color red $ line [V2 0 (-600), V2 0 600]

bitmapTest :: Bitmap -> Frame ()
bitmapTest bmp = blendMode Add $ do
    
    color (fromRGB 1 0 0) $ do
        translate (V2 300 346) $ bitmap bmp -- 'bitmap' creates an action from the bitmap.
    color (fromRGB 0 1 0) $ do
        translate (V2 310 350) $ bitmap bmp -- 'bitmap' creates an action from the bitmap.
    color (fromRGB 0 0 1) $ do
        translate (V2 293 359) $ bitmap bmp -- 'bitmap' creates an action from the bitmap.
    
mouseTest :: Font -> Frame ()
mouseTest font = whenM mouseInWindow $ do
    p <- mousePosition
    l <- mouseButtonL
    r <- mouseButtonR
    let col = case (l, r) of
            (False, False) -> black
            (True, False) -> red
            (False, True) -> blue
            (True, True) -> blend 0.5 red blue
    translate p $ color col $ thickness 4 $ circleOutline 16
    translate p $ color white $ do
        r <- mouseScroll
        text font 48 $ show r

main = runGame Windowed (Box (V2 0 0) (V2 640 480)) $ do
    bmp <- readBitmap "bird.png"
    bmp' <- readBitmap "logo.png"
    font <- loadFont "VL-PGothic-Regular.ttf"
    let bmp' = cropBitmap bmp (128, 128) (64, 64)
    clearColor black
    forkFrame $ preloadBitmap bmp'
    foreverFrame $ do

        bitmapTest bmp
        figureTest
        translate (V2 320 80) $ bitmap bmp' -- You can slice bitmaps using 'cropBitmap'.

        fontTest font
        translate (V2 240 240) $ do
            mouseTest font

            fps <- getFPS

            color black $ text font 15 (show fps)

        whenM (keyDown KeyA) $ translate (V2 300 300) $ color black $ text font 30 "A"
        whenM (keyPress KeyA) $ translate (V2 320 300) $ color black $ text font 30 "B"
        whenM (keyUp KeyA) $ translate (V2 340 300) $ color black $ text font 30 "C"
        whenM (keyDown KeyS) $ takeScreenshot >>= writeBitmap "capture.png"
