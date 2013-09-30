-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.FreeGame.Internal.Raindrop
-- Copyright   :  (C) 2013 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- A portable implementation of lens(<http://hackage.haskell.org/package/lens>)
----------------------------------------------------------------------------
module Graphics.UI.FreeGame.Internal.Raindrop (view, over, biover) where

import Data.Functor.Identity
import Control.Monad.Reader
import Control.Applicative
import Unsafe.Coerce

(#.) :: (b -> c) -> (a -> b) -> (a -> c)
(#.) _ = unsafeCoerce
{-# INLINE (#.) #-}

-- | @'view' :: 'MonadReader' s m => Getting a s a -> m a@
view :: MonadReader s m => ((a -> Const a b) -> (s -> Const a t)) -> m a
view f = asks (getConst #. f Const)
{-# INLINE view #-}

-- | @'over' :: ASetter s t a b -> (a -> b) -> s -> t@
over :: ((a -> Identity b) -> (s -> Identity t)) -> (a -> b) -> s -> t
over l f = runIdentity #. l (Identity #. f)
{-# INLINE over #-}

biover :: ((a -> Identity b) -> (c -> Identity d) -> (s -> Identity t)) -> (a -> b) -> (c -> d) -> s -> t
biover l f g = runIdentity #. l (Identity #. f) (Identity #. g)
{-# INLINE biover #-}