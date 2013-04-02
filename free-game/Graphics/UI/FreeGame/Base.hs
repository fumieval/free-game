{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.FreeGame.Base
-- Copyright   :  (C) 2012-2013 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinsohita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Abstract structures that represents user interfaces
----------------------------------------------------------------------------

module Graphics.UI.FreeGame.Base (
    -- * Types
    UI(..)

    -- * Basic operations
    ,tick
    ,bracket
    ,quit
    ,embedIO
    ,draw
    ,input
    ,_Draw
    ,_Input

) where

import Control.Applicative
import Control.Monad.Free.Class
import Control.Monad.Free
import Control.Monad.Free.Church
import Control.Monad.State.Class
import Control.Monad.IO.Class
import Data.Void

instance Functor i => MonadIO (F (UI p i o)) where
    liftIO = embedIO

instance Functor i => MonadState p (F (UI p i o)) where
    get = wrap $ GetParam return
    put x = wrap $ PutParam x (return ())

instance Functor i => MonadIO (Free (UI p i o)) where
    liftIO = embedIO

instance Functor i => MonadState p (Free (UI p i o)) where
    get = wrap $ GetParam return
    put x = wrap $ PutParam x (return ())

data UI p i o a
    = Tick a
    | EmbedIO (IO a)
    | Bracket (F (UI p i o) a)
    | Quit
    | Draw o a
    | Input (i a)
    | GetParam (p -> a)
    | PutParam p a
    deriving Functor

-- | Finalize the current frame and refresh the screen.
tick :: MonadFree (UI p i o) m => m ()
tick = wrap $ Tick (return ())

-- | Run a Game monad in a Game monad. resources (e.g. pictures) will be released when inner computation is done.
bracket :: MonadFree (UI p i o) m => F (UI p i o) a -> m a
bracket m = wrap $ Bracket $ fmap return m

-- | Break the entire computation.
quit :: MonadFree (UI p i o) m => m Void
quit = wrap Quit

draw :: MonadFree (UI p i o) m => o -> m ()
draw pic = wrap $ Draw pic (return ())

-- | Lift an input functor to　the monad.
input :: (Functor i, MonadFree (UI p i o) m) => i a -> m a
input = wrap . Input . fmap return

embedIO :: (MonadFree (UI p i o) m) => IO a -> m a
embedIO = wrap . EmbedIO . fmap return

-- | A Traversal for 'UI''s drawing
-- @
-- '_Draw' :: Traversal' (UI i o cont) o
-- @
_Draw :: Applicative f => (o -> f o) -> UI p i o cont -> f (UI p i o cont)
_Draw f (Draw p cont) = fmap (`Draw` cont) (f p)
_Draw _ x = pure x

-- | A Traversal for 'UI''s input
-- @
-- '_Input' :: Traversal' (UI i o cont) i
-- @
_Input :: Applicative f => (forall x. i x -> f (i x)) -> UI p i o cont -> f (UI p i o cont)
_Input f (Input i) = fmap Input (f i)
_Input _ x = pure x
