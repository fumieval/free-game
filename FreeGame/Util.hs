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
    -- * Random
    randomness,
    -- * Helper
    degrees,
    radians,
    unitV2,
    angleV2,
    -- * Loading
    loadBitmaps,
    loadBitmapsWith,
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Char
import FreeGame.Data.Bitmap
import Language.Haskell.TH
import Linear
import System.Directory
import System.FilePath
import System.IO.Unsafe
import System.Random
import System.Environment

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
-- On base >= 4.6, file paths to actually load will be respect to the directory of the executable. Otherwise it will be based on the current directory.

#if MIN_VERSION_base(4,6,0)

loadBitmaps :: FilePath -> Q [Dec]
loadBitmaps path = do
    v <- newName "v"
    loadBitmapsWith (lamE [varP v] $
        appsE [varE 'fmap, uInfixE
                    (infixE Nothing (varE '(</>)) (Just (varE v)))
                    (varE '(.))
                    (varE 'takeDirectory)
                , varE 'getExecutablePath]) path

#else

loadBitmaps :: FilePath -> Q [Dec]
loadBitmaps path = loadBitmapsWith (varE 'return) path

#endif

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
