{-# LANGUAGE ExistentialQuantification #-}
module FreeGame.Base (
    Picture(..)
    ,Game(..)
    ,transPicture
) where

import Control.Monad.Free
import Control.Monad
import FreeGame.Bitmap
import FreeGame.Input
import FreeGame.Sound
import System.Random
import qualified Data.ByteString.Lazy as BL
import Data.Unique

data Picture = Image Unique
    | Transform Picture
    | NoTransform Picture
    | Pictures [Picture]
    | Rotate Double Picture
    | Scale Double Picture
    | Translate (Double, Double) Picture 

data Game cont = Tick cont
    | PlaySound Sound cont
    | DrawPicture Picture cont
    | LoadPicture Bitmap (Picture -> cont)
    | AskInput Key (Bool -> cont)
    | LoadSound FilePath (Sound -> cont)
    | EmbedIO (IO cont)
    | Close (Free Game cont)
    | forall a. Random a => Randomness (a, a) (a -> cont)

instance Functor Game where
    fmap f (DrawPicture a cont) = DrawPicture a (f cont)
    fmap f (LoadPicture a cont) = LoadPicture a (f . cont)
    fmap f (PlaySound a cont)   = PlaySound a (f cont)
    fmap f (LoadSound a cont)   = LoadSound a (f . cont)
    fmap f (AskInput a cont)    = AskInput a    (f . cont)
    fmap f (Randomness a cont)  = Randomness a (f . cont)
    fmap f (EmbedIO m) = EmbedIO (fmap f m)
    fmap f (Close m) = Close (fmap f m)
    fmap f (Tick cont) = Tick (f cont)

transPicture :: (Picture -> Picture) -> Game cont -> Game cont
transPicture f (DrawPicture p cont) = DrawPicture (f p) cont
transPicture _ x = x
