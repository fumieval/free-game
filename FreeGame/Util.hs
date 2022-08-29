{-# LANGUAGE TemplateHaskell, CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  FreeGame.Util
-- Copyright   :  (C) 2013 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------

module FreeGame.Util (
    -- * Controlling
    tick,
    foreverTick,
    foreverFrame,
    untick,
    untickInfinite,
    -- * Random
    randomness,
    -- * Helper
    degrees,
    radians,
    unitV2,
    angleV2,
    -- * Loading
    loadPictureFromFile,
    loadBitmaps,
    loadBitmapsWith,
    -- * Keyboard
    charToKey,
    keyChar,
    keySpecial
    ) where

import Control.Monad
import Control.Monad.Free.Class
import Control.Monad.Trans.Iter
import Control.Monad.Trans
import Data.Char
import Data.Void
import FreeGame.Backend.GLFW
import FreeGame.Data.Bitmap
import FreeGame.Class
import FreeGame.Types
import Language.Haskell.TH
import Linear
import System.Directory
import System.FilePath
import System.IO.Unsafe
import System.Random

-- | Delimit the computation to yield a frame.
tick :: (Monad f, MonadFree f m) => m ()
tick = delay (return ())

-- | An infinite loop that run 'tick' every frame after the given action.
foreverTick :: Game a -> Game any
foreverTick m = let m' = foreverTick m in m >> wrap (return m')

-- | @foreverFrame :: Frame a -> Game any@
foreverFrame :: Frame a -> Game any
foreverFrame m = foreverTick (Game $ lift m)

-- | Extract the next frame of the action.
untick :: IterT Frame a -> Frame (Either (IterT Frame a) a)
untick = liftM (either Right Left) . runIterT where

-- | An infinite version of 'untick'.
untickInfinite :: IterT Frame Void -> Frame (IterT Frame Void)
untickInfinite = liftM (either absurd id) . runIterT where

-- | An unit vector with the specified angle.
unitV2 :: Floating a => a -> V2 a
unitV2 t = V2 (cos t) (sin t)

-- | An angle of the given vector.
angleV2 :: RealFloat a => V2 a -> a
angleV2 (V2 a b) = atan2 b a

-- | Get a given range of value.
randomness :: (Random r, MonadIO m) => (r, r) -> m r
randomness r = liftIO $ randomRIO r
{-# INLINE randomness #-}

-- | Convert radians to degrees.
degrees :: Floating a => a -> a
{-# INLINE degrees #-}
degrees x = x / pi * 180

-- | Convert degrees to radians.
radians :: Floating a => a -> a
{-# INLINE radians #-}
radians x = x / 180 * pi

-- | Create a 'Picture' from the given file.
loadPictureFromFile :: (Picture2D p, MonadIO m) => FilePath -> m (p ())
loadPictureFromFile = liftIO . fmap bitmap . readBitmap

-- | The type of the given 'ExpQ' must be @FilePath -> IO FilePath@
-- FIXME: This may cause name duplication if there are multiple non-alphanumeric file names.
loadBitmapsWith :: ExpQ -> FilePath -> Q [Dec]
loadBitmapsWith getFullPath path = do
    loc <- (</>path) <$> takeDirectory <$> loc_filename <$> location
    paths <- runIO $ getFileList loc

    sequence $ do
        p <- paths
        let name = pathToName p
        [ return $ SigD (mkName name) (ConT ''Bitmap)
            , funD (mkName name) [clause [] (normalB $ load name $ loc </> p) []]
            ]
    where
        load name fp = do
            runIO $ putStrLn $ "Defined: " ++ fp ++ " as `" ++ name ++ "'"

            appE (varE 'unsafePerformIO) $ uInfixE (appE getFullPath $ litE $ StringL fp)
                (varE '(>>=))
                (varE 'readBitmap)

-- | Load and define all pictures in the specified directory.
loadBitmaps :: FilePath -> Q [Dec]
loadBitmaps path = loadBitmapsWith (varE 'return) path

getFileList :: FilePath -> IO [FilePath]
getFileList path = do
    allContents <- filter notHidden `fmap` getDirectoryContents path

    files <- filterM (doesFileExist . (path</>)) allContents
    dirs <- filterM (doesDirectoryExist . (path</>)) allContents
    fmap ((files++).concat) $ forM dirs $ \i -> map (i</>) `fmap` getFileList (path</>i)
    where
        notHidden ('.':_) = False
        notHidden _ = True

pathToName :: FilePath -> String
pathToName = ('_':) . map p where
    p c | isAlphaNum c = c
        | otherwise = '_'

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

keyChar :: Keyboard f => Char -> f Bool
keyChar = keyPress . charToKey

{-# DEPRECATED keySpecial "use keyPress instead" #-}
keySpecial :: Keyboard f => Key -> f Bool
keySpecial = keyPress
