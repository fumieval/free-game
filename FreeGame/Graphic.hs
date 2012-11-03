module FreeGame.Graphic where

import Data.Vect.Double
import Data.Unique

data Picture = Image Unique
    | Pictures [Picture]
    | Rotate !Double Picture
    | Scale !Double Picture
    | Translate Vec2 Picture 