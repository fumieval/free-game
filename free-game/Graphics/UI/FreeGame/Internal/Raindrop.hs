module Graphics.UI.FreeGame.Internal.Raindrop (over) where

import Data.Functor.Identity

over :: ((a -> Identity b) -> (s -> Identity t)) -> (a -> b) -> s -> t
over l f = runIdentity . l (Identity . f)
