{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.FreeGame.Util
-- Copyright   :  (C) 2013 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinsohita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------

module Graphics.UI.FreeGame.Util (
    -- * Combinators
    notF,
    (<&&>),
    (<||>),
    -- * Controlling
    untick,
    untickInfinite,
    -- * Random
    randomness,
    -- * Helper
    degrees,
    radians,
    unitV2,
    angleV2,
    sinCos,
    -- * Loading
    loadPictureFromFile,
    loadBitmaps,
    loadBitmapsWith
    ) where
import Control.Monad
import Control.Monad.Free
import Control.Applicative
import Data.Char
import Graphics.UI.FreeGame.Base
import Graphics.UI.FreeGame.Data.Bitmap
import System.Random
import Language.Haskell.TH
import System.Directory
import System.FilePath
import System.IO.Unsafe
import Data.Void
import Linear

-- | A lifted 'not'.
notF :: Functor f => f Bool -> f Bool
notF = fmap not

-- | A lifted '(&&)'.
(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)

-- | A lifted '(||)'
(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)

-- | A unit vector with given angle.
unitV2 :: Floating a => a -> V2 a
unitV2 t = V2 (cos t) (sin t)

-- | An angle of given vector.
angleV2 :: RealFloat a => V2 a -> a
angleV2 (V2 a b) = atan2 b a

-- | Deprecated synonym for 'unitV2'.
sinCos :: Floating a => a -> V2 a
sinCos = unitV2
{-# DEPRECATED sinCos "Use unitV2 instead" #-} 

-- | Extract the next frame of the action.
untick :: (Functor n, MonadFree (UI n) m) => Free (UI n) a -> m (Either (Free (UI n) a) a)
untick (Pure a) = return (Right a)
untick (Free (Tick cont)) = return (Left cont)
untick (Free f) = wrap $ fmap untick f

-- | An infinite version of 'untick'.
untickInfinite :: (Functor n, MonadFree (UI n) m) => Free (UI n) Void -> m (Free (UI n) Void)
untickInfinite = liftM (either id absurd) . untick

-- | Get a given range of value.
randomness :: (Random r, MonadFree (UI n) m) => (r, r) -> m r
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
loadPictureFromFile :: (Picture2D p, MonadFree (UI n) m) => FilePath -> m (p ())
loadPictureFromFile = embedIO . fmap fromBitmap . loadBitmapFromFile

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