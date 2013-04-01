{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.FreeGame.Util
-- Copyright   :  (C) 2013 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinsohita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------

module Graphics.FreeGame.Util (
    untick,
    untickGame,
    untickInfinite,
    randomness,
    degrees,
    radians,
    loadPictureFromFile,
    loadBitmaps,
    loadBitmapsWith) where
import Control.Monad
import Control.Monad.Free
import Control.Applicative
import Data.Char
import Graphics.FreeGame.Base
import Graphics.FreeGame.Data.Bitmap
import System.Random
import Language.Haskell.TH
import System.Directory
import System.FilePath
import System.IO.Unsafe
import Data.Void

untickGame :: MonadFree GameAction m => Free GameAction a -> m (Free GameAction a)
untickGame (Pure a) = return (Pure a)
untickGame (Free (Tick cont)) = return cont
untickGame (Free fm) = wrap $ fmap untickGame fm
{-# DEPRECATED untickGame "use untick or untickInfinite instead" #-}

-- | Run a 'Game' as one frame.
untick :: MonadFree GameAction m => Free GameAction a -> m (Either (Free GameAction a) a)
untick (Pure a) = return (Right a)
untick (Free (Tick cont)) = return (Left cont)
untick (Free f) = wrap $ fmap untick f

-- | An infinite version of 'untick'.
untickInfinite :: MonadFree GameAction m => Free GameAction Void -> m (Free GameAction Void)
untickInfinite = liftM (either id absurd) . untick

whenPressed :: MonadFree GameAction m => Button -> m () -> m ()
whenPressed btn m = do
    s <- getButtonState btn
    when s m

-- | Get a given range of value.
randomness :: (Random r, MonadFree GameAction m) => (r, r) -> m r
randomness = embedIO . randomRIO

-- | Convert radians to degrees.
degrees :: Float -> Float
{-# INLINE degrees #-}
degrees x = x / pi * 180

-- | Convert degrees to radians.
radians :: Float -> Float
{-# INLINE radians #-}
radians x = x / 180 * pi

-- | Create a 'Picture' from the given file.
loadPictureFromFile :: MonadFree GameAction m => FilePath -> m Picture
loadPictureFromFile = embedIO . fmap Bitmap . loadBitmapFromFile

-- | Load and define all pictures in the specified directory.
loadBitmapsWith :: Name -> FilePath -> Q [Dec]
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
            appE (varE 'unsafePerformIO) $ uInfixE (appE (varE getFullPath) (litE $ StringL fp))
                (varE '(>>=))
                (varE 'loadBitmapFromFile)

-- | use with getDataFileName
loadBitmaps :: FilePath -> Q [Dec]
loadBitmaps = loadBitmapsWith 'canonicalizePath

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