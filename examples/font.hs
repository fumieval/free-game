import Graphics.FreeGame.Simple
import Graphics.FreeGame.Data.Color
import Graphics.FreeGame.Data.Font
import Control.Monad

main = do
    font <- loadFont "VL-PGothic-Regular.ttf"
    runSimple defaultGameParam 0 $ \n -> do
        drawPicture $ Translate (Vec2 30 120) $ Colored (halfD red) $ text font 7 ("Counter: " ++ show n)

        drawPicture $ Translate (Vec2 30 240) $ Colored (halfD blue) $ text font 9 "日本語も、美しくレンダリング。"

        withRenderString font 12 "★☆★☆★☆★☆" $ drawPicture . Translate (Vec2 30 360) . Colored yellow

        return $! n + 1