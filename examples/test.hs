{-# LANGUAGE OverloadedStrings #-}
import Graphics.UI.FreeGame
import Control.Applicative
import Control.Monad

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
        $ text font 17 "Hello, World" -- Use 'text font size string' to draw a string.

bitmapTest :: Bitmap -> Game ()
bitmapTest bmp = do
    translate (V2 300 350) $ fromBitmap bmp -- 'fromBitmap' creates an action from the bitmap.
    translate (V2 100 350) $ fromBitmap (cropBitmap bmp (32, 32) (0, 0)) -- You can slice bitmaps using 'cropBitmap'.

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

main = runGame def {
        _framePerSecond = 60
        , _windowTitle = "free-game"
        , _windowed = True -- If you want to run in full screen mode, specify False.
        , _cursorVisible = True -- Whether the application shows the cursor
        , _clearColor = white -- Background color
        , _windowRegion = BoundingBox 0 0 640 480 -- The region of the window 
    } $ do
    font <- loadFont "VL-PGothic-Regular.ttf"
    bmp <- loadBitmapFromFile "logo.png"
    foreverTick $ do
        bitmapTest bmp
        figureTest
        fontTest font
        translate (V2 240 240) $ mouseTest