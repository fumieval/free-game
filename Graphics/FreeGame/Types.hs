module Graphics.FreeGame.Types (BoundingBox(..)) where
import Data.Vect
data BoundingBox = BoundingBox
    { topLeft :: Vec2
    , bottomRight :: Vec2
    }