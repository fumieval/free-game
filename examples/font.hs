import Graphics.FreeGame.Simple
import Graphics.FreeGame.Data.Color
import Graphics.FreeGame.Data.Font
import Control.Monad

main = do
    font <- loadFont "VL-PGothic-Regular.ttf"
    print $ fontBoundingBox font
    runSimple defaultGameParam 0 $ \n -> do
        drawPicture $ Translate (Vec2 30 120) $ Colored (halfD red) $ text font 14 ("Counter: " ++ show n)

        drawPicture $ Translate (Vec2 30 240) $ Colored (halfD blue) $ text font 20 "日本語も、美しくレンダリング。"

        -- drawPicture $ Translate (Vec2 30 (240 +  * 20)) $ Colored (halfD green) $ text font 20 "___"

        return $! n + 1