module Graphics.FreeGame.Lens where

import Graphics.FreeGame.Base

makeLenses ''GameParam

translation :: Lens' Picture Vec2

rotation :: Lens' Picture Float

scaling :: Lens' Picture Vec2

color :: Lens' Picture Vec2
