-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.FreeGame.Input
-- Copyright   :  (C) 2013 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Graphics.FreeGame.UI.Input (
    Input(..)
    ) where

import Data.Vect

runInput :: Input x -> IO x
runInput (InputReturn x) = return x
runInput (InputBind mf m) = runInput mf <*> runInput m
runInput KeyF1
runInput KeyF2 

data Input x where
    InputPure :: a -> Input a
    InputAp :: Input (a -> b) -> Input a -> Input b
    Previous :: Input a -> Input a
    MousePosition :: Input Vec2
    MouseWheel :: Input Int
    MouseLeft :: Input Bool
    MouseRight :: Input Bool
    MouseMiddle :: Input Bool
    KeyChar :: Char -> Input Bool
    KeySpace :: Input Bool
    KeyEsc :: Input Bool
    KeyF1 :: Input Bool
    KeyF2 :: Input Bool
    KeyF3 :: Input Bool
    KeyF4 :: Input Bool
    KeyF5 :: Input Bool
    KeyF6 :: Input Bool
    KeyF7 :: Input Bool
    KeyF8 :: Input Bool
    KeyF9 :: Input Bool
    KeyF10 :: Input Bool
    KeyF11 :: Input Bool
    KeyF12 :: Input Bool
    KeyLeftShift :: Input Bool
    KeyRightShift :: Input Bool
    KeyLeftControl :: Input Bool
    KeyRightControl :: Input Bool
    KeyUp :: Input Bool
    KeyDown :: Input Bool
    KeyLeft :: Input Bool
    KeyRight :: Input Bool
    KeyTab :: Input Bool
    KeyEnter :: Input Bool
    KeyBackspace :: Input Bool
    KeyInsert :: Input Bool
    KeyDelete :: Input Bool
    KeyPageUp :: Input Bool
    KeyPageDown :: Input Bool
    KeyHome :: Input Bool
    KeyEnd :: Input Bool
    KeyPad0 :: Input Bool
    KeyPad1 :: Input Bool
    KeyPad2 :: Input Bool
    KeyPad3 :: Input Bool
    KeyPad4 :: Input Bool
    KeyPad5 :: Input Bool
    KeyPad6 :: Input Bool
    KeyPad7 :: Input Bool
    KeyPad8 :: Input Bool
    KeyPad9 :: Input Bool
    KeyPadDivide :: Input Bool
    KeyPadMultiply :: Input Bool
    KeyPadSubtract :: Input Bool
    KeyPadAdd :: Input Bool
    KeyPadDecimal :: Input Bool
    KeyPadEqual :: Input Bool
    KeyPadEnter :: Input Bool

instance Functor Input where
    fmap f m = InputPure f `InputAp` m

instance Applicative Input where
    pure = InputPure
    (<*>) = InputAp