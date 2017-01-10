import FreeGame

data StateData = StateData { _counter :: Int
                           , _font :: Font
                           , _exiting :: Bool
                           }
    
calc :: StateData -> Game StateData
calc state = do
    let cnt = 1 + _counter state
    exiting <- keyPress KeyEscape
    return state{_counter = cnt, _exiting = exiting}

isExiting :: StateData -> Game Bool
isExiting = return . _exiting

drawer :: StateData -> Game ()
drawer state = do
    let fnt = _font state
        cnt = _counter state

    translate (V2 100 240) . color green . text fnt 15 $ show cnt
    translate (V2 340 240) . rotateD (fromIntegral cnt) . color red $ text fnt 70 "Test"
    tick

main = runGame Windowed (Box (V2 0 0) (V2 640 480)) $ do
    font <- loadFont "VL-PGothic-Regular.ttf"
    clearColor black
    let state = StateData{_counter=0, _font=font, _exiting=False}
    controlledGame calc isExiting drawer state
