import Graphics.FreeGame.Simple
import Graphics.FreeGame.Data.Color
import Graphics.FreeGame.Data.Font
import Control.Monad

main = do
    font <- loadFont "VL-PGothic-Regular.ttf"
    runSimple defaultGameParam 0 $ \n -> do
        drawPicture $ Translate (Vec2 30 120) $ text font 7 (halfD red) ("Counter: " ++ show n)

        drawPicture $ Translate (Vec2 30 240) $ text font 9 (halfD blue) "日本語も、美しくレンダリング。"

        withRenderString font 12 yellow "★☆★☆★☆★☆" $ drawPicture . Translate (Vec2 30 360)

        return $! n + 1