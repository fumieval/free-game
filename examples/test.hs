{-# LANGUAGE OverloadedStrings #-}
import FreeGame
import Control.Applicative
import Control.Monad
import Control.Monad.State

figureTest :: Game ()
figureTest = do
    colored cyan -- 'colored' gives a color to the action.
        $ line [V2 80 80, V2 160 160] -- 'line' draws line through the given coordinates.

    colored green $ polygon [V2 20 0, V2 100 20, V2 90 60, V2 30 70]

    colored blue
        $ translate (V2 0 200) -- 'translate' moves the action.
        $ rotateD 45 -- 'rotateD' rotates the action. (in degrees).
        $ scale 2 -- 'scale' resizes the action. You can also specify as (V2 x y).
        $ polygonOutline [V2 20 0, V2 100 20, V2 50 60]

    colored magenta
        $ thickness 3 -- 'thickness' sets the thickness of the lines.
        $ translate (V2 100 300)
        $ circleOutline 50

fontTest :: Font -> Game ()
fontTest font = do
    translate (V2 100 300) $ colored black
        $ text font 48 "Hello, World" -- Use 'text font size string' to draw a string.

bitmapTest :: Bitmap -> Game ()
bitmapTest bmp = do
    colored (Color 1 1 1 0.5) $ do
        translate (V2 300 350) $ bitmap bmp -- 'bitmap' creates an action from the bitmap.
        translate (V2 300 360) $ bitmap bmp
    translate (V2 100 350) $ bitmap (cropBitmap bmp (32, 32) (0, 0)) -- You can slice bitmaps using 'cropBitmap'.

mouseTest :: Game ()
mouseTest = do
    p <- mousePosition
    l <- mouseButtonL
    r <- mouseButtonR
    let color = case (l, r) of
            (False, False) -> black
            (True, False) -> red
            (False, True) -> blue
            (True, True) -> blend 0.5 red blue
    translate p $ colored color $ thickness 4 $ circleOutline 16

main = runGame $ do
    bmp <- embedIO $ readBitmap "logo.png"
    bmp' <- embedIO $ readBitmap "Icon.png"
    font <- embedIO $ loadFontFromFile "VL-PGothic-Regular.ttf"
    flip execStateT bmp' $ foreverTick $ do
        lift $ bitmapTest bmp'
        whenM (keyDown KeyS) $ do
            lift (lift takeScreenshot) >>= put
        lift figureTest

        lift $ fontTest font


        translate (V2 240 240) $ do
            lift $ mouseTest
            get >>= scale 0.25 . bitmap