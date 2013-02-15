-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.FreeGame.Input
-- Copyright   :  (C) 2013 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinsohita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Graphics.FreeGame.Input (
    Button(..)
    -- * Deprecated
    , Key
    , MouseState(..)
    ) where

import Data.Vect

-- | Buttons.
data Button
    = KeyChar Char
    | KeySpace
    | KeyEsc
    | KeyF1
    | KeyF2
    | KeyF3
    | KeyF4
    | KeyF5
    | KeyF6
    | KeyF7
    | KeyF8
    | KeyF9
    | KeyF10
    | KeyF11
    | KeyF12
    | KeyLeftShift
    | KeyRightShift
    | KeyLeftControl
    | KeyRightControl
    | KeyUp
    | KeyDown
    | KeyLeft
    | KeyRight
    | KeyTab
    | KeyEnter
    | KeyBackspace
    | KeyInsert
    | KeyDelete
    | KeyPageUp
    | KeyPageDown
    | KeyHome
    | KeyEnd
    | KeyPad0
    | KeyPad1
    | KeyPad2
    | KeyPad3
    | KeyPad4
    | KeyPad5
    | KeyPad6
    | KeyPad7
    | KeyPad8
    | KeyPad9
    | KeyPadDivide
    | KeyPadMultiply
    | KeyPadSubtract
    | KeyPadAdd
    | KeyPadDecimal
    | KeyPadEqual
    | KeyPadEnter
    | MouseLeft
    | MouseRight
    | MouseMiddle
    deriving (Show, Eq, Ord)

{-# DEPRECATED MouseState "no longer needed" #-}
data MouseState = MouseState
    { mousePosition :: Vec2
    , leftButton :: Bool
    , middleButton :: Bool
    , rightButton :: Bool
    , mouseWheel :: Int
    } deriving Show

{-# DEPRECATED Key "use Button instead" #-}
-- Deprecated synonym for 'Button'.
type Key = Button