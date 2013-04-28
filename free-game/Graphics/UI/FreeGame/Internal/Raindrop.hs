module Graphics.UI.FreeGame.Internal.Raindrop (view, over) where

import Data.Functor.Identity
import Control.Monad.Reader
import Control.Applicative

view :: MonadReader s m => ((a -> Const a b) -> (s -> Const a t)) -> m a
view f = ask >>= return . getConst . f Const

over :: ((a -> Identity b) -> (s -> Identity t)) -> (a -> b) -> s -> t
over l f = runIdentity . l (Identity . f)
