{-# LANGUAGE FlexibleInstances, FlexibleContexts, Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  FreeGame.Backend.GLFW
-- Copyright   :  (C) 2013 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module FreeGame.Backend.GLFW (runGame) where
import Control.Applicative
import Control.Artery
import Control.Bool
import Control.Concurrent.MVar
import Control.Monad.Free.Church
import Control.Monad.Trans.Iter
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.IORef
import Data.Reflection
import Data.Default
import FreeGame.Class
import FreeGame.Data.Bitmap
import FreeGame.Data.Wave
import FreeGame.Internal.Finalizer
import FreeGame.UI
import FreeGame.Types
import Linear
import qualified Data.IntMap as IM
import qualified Data.Map as Map
import qualified FreeGame.Internal.GLFW as G
import qualified Graphics.UI.GLFW as GLFW
import DSP.Artery.IO
runGame :: IterT (F UI) a -> IO (Maybe a)
runGame m_ = G.withGLFW 60 (BoundingBox 0 0 640 480)
    $ \sys -> do
        texs <- newIORef IM.empty
        str <- Stream <$> newMVar []
        keyBuffer <- newIORef $ Map.fromList $ zip [minBound..] (repeat False)
        mouseBuffer <- newIORef $ Map.fromList $ zip [0..7] (repeat False)
        keyBuffer' <- newIORef undefined
        mouseBuffer' <- newIORef undefined
        GLFW.setKeyCallback (G.theWindow sys) $ Just
            $ \_ key _ st _ -> modifyIORef' keyBuffer (Map.insert (toEnum $ fromEnum key) (G.fromKeyState st))
        GLFW.setMouseButtonCallback (G.theWindow sys) $ Just
            $ \_ btn st _ -> modifyIORef' mouseBuffer (Map.insert (fromEnum btn) (G.fromMouseButtonState st))
        -- setScrollCallback (G.theWindow sys)
        withStream def (const $ streamTap str) $ runFinalizerT
            $ give (RefKeyStates keyBuffer)
            $ give (RefMouseButtonStates mouseBuffer)
            $ give (Previous (RefKeyStates keyBuffer'))
            $ give (Previous (RefMouseButtonStates mouseBuffer'))
            $ give str
            $ give (TextureStorage texs)
            $ give sys
            $ go m_
    where
        go :: (Given Stream
            , Given G.System
            , Given TextureStorage
            , Given KeyStates
            , Given MouseButtonStates
            , Given (Previous KeyStates)
            , Given (Previous MouseButtonStates)
            ) => IterT (F UI) a -> FinalizerT IO (Maybe a)
        go m = do
            liftIO $ readIORef (getKeyStates given) >>= writeIORef (getKeyStates (getPrevious given))
            liftIO $ readIORef (getMouseButtonStates given) >>= writeIORef (getMouseButtonStates (getPrevious given))
            liftIO $ G.beginFrame given
            r <- iterM runUI $ runIterT m
            ifThenElseM (liftIO $ G.endFrame given) (return Nothing) $ case r of
                Iter cont -> go cont
                Pure a -> return (Just a)

newtype TextureStorage = TextureStorage { getTextureStorage :: IORef (IM.IntMap G.Texture) }

type DrawM = ReaderT (Location ()) IO

newtype Stream = Stream { getStream :: MVar [V2 Float] }

newtype Previous a = Previous { getPrevious :: a }

newtype KeyStates = RefKeyStates { getKeyStates :: IORef (Map.Map Key Bool) }
newtype MouseButtonStates = RefMouseButtonStates { getMouseButtonStates :: IORef (Map.Map Int Bool) }

streamTap :: Stream -> Artery IO () (V2 Float)
streamTap (Stream mstr) = effectful $ const $ do
    st <- takeMVar mstr
    case st of
        [] -> putMVar mstr st >> return zero
        (x:xs) -> putMVar mstr xs >> return x

runUI :: forall a.
    ( Given Stream
    , Given G.System
    , Given TextureStorage
    , Given KeyStates
    , Given MouseButtonStates
    , Given (Previous KeyStates)
    , Given (Previous MouseButtonStates)
    ) => UI (FinalizerT IO a) -> FinalizerT IO a
runUI (Draw m) = do
    v <- liftIO $ newIORef (return () :: IO ())
    cont <- liftIO $ give v $ runReaderT (m :: DrawM (FinalizerT IO a)) (Location id id)
    liftIO (readIORef v) >>= finalizer
    cont
runUI (FromFinalizer m) = join m
runUI (Configure _ cont) = cont
runUI (KeyStates cont) = liftIO (readIORef $ getKeyStates given) >>= cont
runUI (MouseButtons cont) = liftIO (readIORef $ getMouseButtonStates given) >>= cont
runUI (PreviousKeyStates cont) = liftIO (readIORef $ getKeyStates $ getPrevious given) >>= cont
runUI (PreviousMouseButtons cont) = liftIO (readIORef $ getMouseButtonStates $ getPrevious given) >>= cont
-- runUI _ _ (MouseWheel cont) = GLFW.getMouseWheel >>= cont
runUI (MousePosition cont) = do
    (x, y) <- liftIO $ GLFW.getCursorPos (G.theWindow given)
    cont $ V2 x y
runUI (Play (WaveData w _) cont) = do
    liftIO $ takeMVar (getStream given) >>= \st -> putMVar (getStream given) $ go st w
    cont
    where
        go (x:xs) (y:ys) = x + y : go xs ys
        go xs [] = xs
        go [] ys = ys

mapReaderWith :: (s -> r) -> (m a -> n b) -> ReaderT r m a -> ReaderT s n b
mapReaderWith f g m = ReaderT $ \s -> g (runReaderT m (f s))

instance Affine DrawM where
    translate v = mapReaderWith (translate v) (G.translate v)
    rotateD t = mapReaderWith (rotateD t) (G.rotateD t)
    rotateR t = let t' = t * pi / 180 in mapReaderWith (rotateD t') (G.rotateD t')
    scale v = mapReaderWith (scale v) (G.scale v)

instance (Given (IORef (IO ())), Given TextureStorage) => Picture2D DrawM where
    bitmap bmp@(BitmapData _ (Just h)) = liftIO $ do
        m <- readIORef (getTextureStorage given)
        case IM.lookup h m of
            Just t -> G.drawTexture t
            Nothing -> do
                t <- G.installTexture bmp
                writeIORef (getTextureStorage given) $ IM.insert h t m
                G.drawTexture t
                modifyIORef given $ (>>G.releaseTexture t) . (>>modifyIORef' (getTextureStorage given) (IM.delete h))
    bitmap bmp@(BitmapData _ Nothing) = liftIO $ G.installTexture bmp >>= G.drawTexture
    circle r = liftIO (G.circle r)
    circleOutline r = liftIO (G.circleOutline r)
    polygon vs = liftIO (G.polygon vs)
    polygonOutline vs = liftIO (G.polygonOutline vs)
    line vs = liftIO (G.line vs)
    thickness t = mapReaderWith id (G.thickness t)
    color c = mapReaderWith id (G.color c)

instance Local DrawM where
    getLocation = asks coerceLocation
