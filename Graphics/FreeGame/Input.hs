-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.FreeGame.Base
-- Copyright   :  (C) 2012 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinsohita <fumiexcel@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
-- Common key and mouse inputs
----------------------------------------------------------------------------

module Graphics.FreeGame.Input where

data MouseState = MouseState { position :: (Double, Double)
        , leftButton :: Bool
        , middleButton :: Bool
        , rightButton :: Bool
        , mouseWheel :: Int
        } deriving (Eq, Ord, Show)

data Key
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
        | KeyF13
        | KeyF14
        | KeyF15
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
        | KeyNumLock
        | KeyBegin
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
        deriving (Show, Eq, Ord)
