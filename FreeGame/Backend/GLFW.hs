{-# LANGUAGE FlexibleInstances, FlexibleContexts, Rank2Types, ScopedTypeVariables #-}
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
module FreeGame.GUI.GLFW () where
import Control.Applicative
import Control.Artery
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Free.Church
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.IORef
import Data.Reflection
import FreeGame.Class
import FreeGame.Data.Bitmap
import FreeGame.Data.Wave
import FreeGame.Internal.Finalizer
import FreeGame.UI
import Linear
import qualified Data.IntMap as IM
import qualified FreeGame.Internal.GLFW as G
import qualified Graphics.UI.GLFW as GLFW
import System.IO.Unsafe

{-
runGame :: IterT (F UI) a -> IO (Maybe a)
runGame m = G.withGLFW $ runFinalizerT $ go m where
    go m = do
        r <- runIterT
-}

newtype TextureStorage = TextureStorage { getTextureStorage :: IORef (IM.IntMap G.Texture) }

type DrawM = ReaderT (ViewPort ()) IO

data Stream = Stream [V2 Float]

streamTap :: MVar Stream -> Artery IO () (V2 Float)
streamTap mstr = effectful $ const $ do
    Stream st <- takeMVar mstr
    case st of
        [] -> putMVar mstr (Stream st) >> return zero
        (x:xs) -> putMVar mstr (Stream xs) >> return x

runUI :: forall a. Given TextureStorage => GLFW.Window -> MVar Stream -> UI (FinalizerT IO a) -> FinalizerT IO a
runUI _ _ (Draw m) = do
    v <- liftIO $ newIORef (return () :: IO ())
    cont <- liftIO $ give v $ runReaderT (m :: DrawM (FinalizerT IO a)) (ViewPort id id)
    cont

runUI w _ (KeyState k cont) = liftIO (GLFW.getKey w $ toEnum $ fromEnum k) >>= cont . G.fromKeyState
runUI w _ (MouseButtonL cont) = liftIO (GLFW.getMouseButton w GLFW.MouseButton'1) >>= cont . G.fromMouseButtonState
runUI w _ (MouseButtonR cont) = liftIO (GLFW.getMouseButton w GLFW.MouseButton'2) >>= cont . G.fromMouseButtonState
runUI w _ (MouseButtonM cont) = liftIO (GLFW.getMouseButton w GLFW.MouseButton'3) >>= cont . G.fromMouseButtonState
-- runUI _ _ (MouseWheel cont) = GLFW.getMouseWheel >>= cont
runUI w _ (MousePosition cont) = do
    (x, y) <- liftIO $ GLFW.getCursorPos w
    cont $ V2 x y
runUI _ mstr (Play (WaveData w _) cont) = do
    liftIO $ takeMVar mstr >>= \(Stream st) -> putMVar mstr $ Stream (go st w)
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
                modifyIORef given $ (>>G.releaseTexture t) . (>>modifyIORef (getTextureStorage given) (IM.delete h))
    bitmap bmp@(BitmapData _ Nothing) = liftIO $ G.installTexture bmp >>= G.drawTexture
    circle r = liftIO (G.circle r)
    circleOutline r = liftIO (G.circleOutline r)
    polygon vs = liftIO (G.polygon vs)
    polygonOutline vs = liftIO (G.polygonOutline vs)
    line vs = liftIO (G.line vs)
    thickness t = mapReaderWith id (G.thickness t)
    colored c= mapReaderWith id (G.colored c)

instance Local DrawM where
    getViewPort = asks coerceViewPort
