{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  FreeGame.Types
-- Copyright   :  (C) 2013 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module FreeGame.Types (
    Time
    , Vec2
    , WindowMode(..)
    , BoundingBox2
    , Key(..)
    , charToKey
    , BlendMode(..)
    ) where

import Linear
import Data.Typeable
import Data.BoundingBox
import Data.Char

type Time = Double

data WindowMode = Windowed | Resizable | FullScreen deriving (Show, Eq, Ord, Read)

type Vec2 = V2 Double

type BoundingBox2 = Box V2 Double

data Key =
      KeyUnknown
    | KeySpace
    | KeyApostrophe
    | KeyComma
    | KeyMinus
    | KeyPeriod
    | KeySlash
    | Key0
    | Key1
    | Key2
    | Key3
    | Key4
    | Key5
    | Key6
    | Key7
    | Key8
    | Key9
    | KeySemicolon
    | KeyEqual
    | KeyA
    | KeyB
    | KeyC
    | KeyD
    | KeyE
    | KeyF
    | KeyG
    | KeyH
    | KeyI
    | KeyJ
    | KeyK
    | KeyL
    | KeyM
    | KeyN
    | KeyO
    | KeyP
    | KeyQ
    | KeyR
    | KeyS
    | KeyT
    | KeyU
    | KeyV
    | KeyW
    | KeyX
    | KeyY
    | KeyZ
    | KeyLeftBracket
    | KeyBackslash
    | KeyRightBracket
    | KeyGraveAccent
    | KeyWorld1
    | KeyWorld2
    | KeyEscape
    | KeyEnter
    | KeyTab
    | KeyBackspace
    | KeyInsert
    | KeyDelete
    | KeyRight
    | KeyLeft
    | KeyDown
    | KeyUp
    | KeyPageUp
    | KeyPageDown
    | KeyHome
    | KeyEnd
    | KeyCapsLock
    | KeyScrollLock
    | KeyNumLock
    | KeyPrintScreen
    | KeyPause
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
    | KeyF16
    | KeyF17
    | KeyF18
    | KeyF19
    | KeyF20
    | KeyF21
    | KeyF22
    | KeyF23
    | KeyF24
    | KeyF25
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
    | KeyPadDecimal
    | KeyPadDivide
    | KeyPadMultiply
    | KeyPadSubtract
    | KeyPadAdd
    | KeyPadEnter
    | KeyPadEqual
    | KeyLeftShift
    | KeyLeftControl
    | KeyLeftAlt
    | KeyLeftSuper
    | KeyRightShift
    | KeyRightControl
    | KeyRightAlt
    | KeyRightSuper
    | KeyMenu
    deriving (Enum, Eq, Ord, Read, Show, Typeable, Bounded)

charToKey :: Char -> Key
charToKey ch
    | isAlpha ch = toEnum $ fromEnum KeyA + fromEnum ch - fromEnum 'A'
    | isDigit ch = toEnum $ fromEnum Key0 + fromEnum ch - fromEnum '0'
charToKey '-' = KeyMinus
charToKey ',' = KeyComma
charToKey '.' = KeyPeriod
charToKey '/' = KeySlash
charToKey ' ' = KeySpace
charToKey '\'' = KeyApostrophe
charToKey '\\' = KeyBackslash
charToKey '=' = KeyEqual
charToKey ';' = KeySemicolon
charToKey '[' = KeyLeftBracket
charToKey ']' = KeyRightBracket
charToKey '`' = KeyGraveAccent
charToKey '\n' = KeyEnter
charToKey '\r' = KeyEnter
charToKey '\t' = KeyTab
charToKey _ = KeyUnknown

data BlendMode = Normal
    | Inverse
    | Add
    | Multiply
    | Screen
    deriving (Enum, Eq, Ord, Read, Show, Typeable)