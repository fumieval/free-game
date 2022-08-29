import FreeGame
import Control.Monad.Trans

data StateData = StateData { _counter :: Int
                           , _font :: Font
                           }

loop :: StateData -> Game ()
loop state = do
    let fnt = _font state
        cnt = 1 + _counter state
        state' = state{_counter = cnt}

    translate (V2 100 240) . color green . text fnt 15 $ show cnt
    translate (V2 340 240) . rotateD (fromIntegral cnt) . color red $ text fnt 70 "Test"

    key <- keyPress KeyEscape
    unless key $ tick >> loop state'

main = runGame Windowed (Box (V2 0 0) (V2 640 480)) $ do
    font <- loadFont "examples/VL-PGothic-Regular.ttf"
    clearColor black
    let state = StateData{_counter=0, _font=font}
    loop state
