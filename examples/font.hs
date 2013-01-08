import Graphics.FreeGame.Simple
import Graphics.FreeGame.Data.Color
import Graphics.FreeGame.Data.Font
import Control.Monad

main = do
    font <- loadFont "VL-PGothic-Regular.ttf" 1
    runSimple defaultGameParam 0 $ \n -> do
        withRenderString font (halfD red) ("Counter: " ++ show n) $ drawPicture . Translate (Vec2 0 120)
        return $ n + 1