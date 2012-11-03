{-# LANGUAGE ExistentialQuantification #-}
module FreeGame.Base (ImageData, Game(..), transPicture) where

import Control.Monad
import FreeGame.Graphic
import FreeGame.Input
import FreeGame.Sound
import System.Random
import Codec.Picture.Repa
import qualified Data.ByteString.Lazy as BL

type ImageData = Img RGBA

data Game cont = Tick cont
    | PlaySound Sound cont
    | DrawPicture Picture cont
    | LoadImage ImageData (Picture -> cont)
    | AskInput Key (Bool -> cont)
    | LoadSound FilePath (WaveData -> cont)
    | EmbedIO (IO cont)
    | forall a. Random a => Randomness (a, a) (a -> cont)

instance Functor Game where
    fmap f (DrawPicture a cont) = DrawPicture a (f cont)
    fmap f (LoadImage a cont)   = LoadImage a (f . cont)
    fmap f (PlaySound a cont)   = PlaySound a (f cont)
    fmap f (LoadSound a cont)   = LoadSound a (f . cont)
    fmap f (AskInput a cont)    = AskInput a    (f . cont)
    fmap f (Randomness a cont)  = Randomness a (f . cont)
    fmap f (EmbedIO m) = EmbedIO (fmap f m)
    fmap f (Tick cont) = Tick (f cont)

transPicture :: (Picture -> Picture) -> Game cont -> Game cont
transPicture f (DrawPicture p cont) = DrawPicture (f p) cont
transPicture _ x = x
