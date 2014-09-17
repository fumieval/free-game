{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  FreeGame.Backend.GLFW
-- Copyright   :  (C) 2013-2014 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module FreeGame.Backend.GLFW where
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Applicative
import Control.Artery
import Data.IORef
import Data.Reflection
import qualified Data.Vector as V
import FreeGame.Picture
import FreeGame.Component
import FreeGame.Data.Bitmap
import FreeGame.Types
import Linear
import qualified System.PortAudio as PA
-- #if (MIN_VERSION_containers(0,5,0))
import qualified Data.IntMap.Strict as IM
import qualified FreeGame.Internal.GLFW as G
import qualified Graphics.UI.GLFW as GLFW
import Unsafe.Coerce
import Control.Concurrent
import Linear.V
import GHC.Prim
import Control.Exception

newtype System s a = System (ReaderT (Foundation s) IO a) deriving (Functor, Applicative, Monad)

unSystem :: Foundation s -> System s a -> IO a
unSystem f m = unsafeCoerce m f

mkSystem :: (Foundation s -> IO a) -> System s a
mkSystem = unsafeCoerce

runGame :: WindowMode -> BoundingBox2 -> (forall s. System s a) -> IO ()
runGame mode box m = do
    sys <- G.beginGLFW mode box
    f <- Foundation
        <$> newMVar 0
        <*> pure 44100 -- FIX THIS
        <*> newIORef IM.empty
        <*> newIORef IM.empty
        <*> newIORef Nothing
        <*> newIORef IM.empty
        <*> newIORef IM.empty
        <*> newMVar 0
        <*> pure sys
        <*> newIORef IM.empty
        <*> newEmptyMVar
    ref <- newEmptyMVar
    _ <- flip forkFinally (either throwIO (putMVar ref)) $ unSystem f m
    PA.with undefined undefined undefined (audioProcess f) $ liftIO $ do
        GLFW.setTime 0
        runGraphic f
    G.endGLFW sys
    return ()

data Foundation s = Foundation
    { newComponentId :: MVar Int
    , sampleRate :: Double
    , cores :: IORef (IM.IntMap (MVar (Component Any (System s))))
    , coreGraphic :: IORef (IM.IntMap Any) -- rely on `invoke`
    , coreAudio :: IORef (Maybe (Int, Any))
    , coreKeyboard :: IORef (IM.IntMap Any)
    , coreMouse :: IORef (IM.IntMap Any)
    , theTime :: MVar Double
    , theSystem :: G.System
    , textures :: IORef (IM.IntMap G.Texture)
    , theEnd :: MVar ()
    }

drawPicture :: forall a. Given TextureStorage => Picture a -> IO a
drawPicture (Picture m) = runReaderT (m :: DrawM a) (Location id id)

runGraphic :: Foundation s -> IO ()
runGraphic fo = do
    Just t <- GLFW.getTime
    G.beginFrame (theSystem fo)
    pics <- broadcast fo (coreGraphic fo) $ \s -> s t
    give (TextureStorage (textures fo)) $ mapM_ drawPicture pics
    b <- G.endFrame (theSystem fo)
    tryTakeMVar (theEnd fo) >>= \case
        Just _ -> return ()
        _ | b -> putMVar (theEnd fo) ()
          | otherwise -> runGraphic fo

v2v2 :: V2 Float -> V 2 Float
v2v2 (V2 x y) = case fromVector $ V.fromList [x, y] of
    Just a -> a
    Nothing -> zero

audioProcess :: Foundation s -> Artery IO (PA.Chunk (V 0 Float)) [V 2 Float]
audioProcess fo = effectful $ \(PA.Chunk n _) -> readIORef (coreAudio fo) >>= \case
    Nothing -> return $ fmap v2v2 $ replicate n 0
    Just (j, pull) -> do
        m <- readIORef $ cores fo
        let dt = fromIntegral n / sampleRate fo
        fmap (fmap v2v2) $ push fo (m IM.! j) (unsafeCoerce pull dt n)

push :: Foundation s -> MVar (Component Any (System s)) -> e a -> IO a
push fo mc e = do
    c0 <- takeMVar mc
    (a, c) <- unSystem fo $ runComponent c0 (unsafeCoerce e)
    putMVar mc c
    return a

broadcast :: Foundation s
    -> IORef (IM.IntMap Any)
    -> something
    -> IO [a]
broadcast fo rfs e = do
    cs <- readIORef (cores fo)
    fs <- readIORef rfs
    forM (IM.assocs fs) $ \(j, f) -> push fo (cs IM.! j) (unsafeCoerce e (unsafeCoerce f))

keyCallback :: Foundation s -> GLFW.KeyCallback
keyCallback fo _ k _ st _ = void $ broadcast fo (coreKeyboard fo)
    $ \s -> s (toEnum . fromEnum $ k :: Key) (GLFW.KeyState'Released /= st)

mouseButtonCallback :: Foundation s -> GLFW.MouseButtonCallback
mouseButtonCallback fo _ btn st _ = void $ broadcast fo (coreMouse fo)
    $ \(s, _, _) -> s (fromEnum btn) (GLFW.MouseButtonState'Released /= st)

cursorPosCallback :: Foundation s -> GLFW.CursorPosCallback
cursorPosCallback fo _ x y = void $ broadcast fo (coreMouse fo)
    $ \(_, s, _) -> s (V2 x y)

scrollCallback :: Foundation s -> GLFW.ScrollCallback
scrollCallback fo _ x y = void $ broadcast fo (coreMouse fo)
    $ \(_, _, s) -> s (V2 x y)

assimilate :: Control s e -> e a -> e a
assimilate _ = id

instance MonadIO (System s) where
    liftIO m = mkSystem $ const m
    {-# INLINE liftIO #-}

instance (s0 ~ s) => MonadSystem s0 (System s) where
    type Base (System s) = System s
    Control i .- e = mkSystem $ \fo -> do
        m <- readIORef $ cores fo
        push fo (m IM.! i) (unsafeCoerce e)
    invoke c = mkSystem $ \fo -> do
        n <- takeMVar $ newComponentId fo
        mc <- newMVar (unsafeCoerce c)
        modifyIORef (cores fo) $ IM.insert n mc
        putMVar (newComponentId fo) (n + 1)
        return (Control n)
    connectGraphic con@(Control i) = mkSystem $ \fo -> modifyIORef (coreGraphic fo)
        $ IM.insert i (unsafeCoerce $ assimilate con . pullGraphic)
    connectAudio con@(Control i) = mkSystem $ \fo -> do
        writeIORef (coreAudio fo) $ Just (i, unsafeCoerce $ (assimilate con .) . pullAudio)
        return ()
    disconnectGraphic (Control i) = mkSystem $ \fo -> modifyIORef (coreGraphic fo) $ IM.delete i
    disconnectAudio (Control _) = mkSystem $ \fo -> do
        _ <- writeIORef (coreAudio fo) Nothing
        return ()
    connectKeyboard con@(Control i) = mkSystem
        $ \fo -> modifyIORef (coreKeyboard fo)
        $ IM.insert i (unsafeCoerce $ (assimilate con .) . keyEvent)
    disconnectKeyboard (Control i) = mkSystem $ \fo -> modifyIORef (coreKeyboard fo) $ IM.delete i
    connectMouse con@(Control i) = mkSystem
        $ \fo -> modifyIORef (coreMouse fo) $ IM.insert i (unsafeCoerce
            ( (assimilate con .) . mouseButtonEvent
            , assimilate con . cursorEvent
            , assimilate con . scrollEvent))
    disconnectMouse (Control i) = mkSystem $ \fo -> modifyIORef (coreMouse fo) $ IM.delete i
    wait dt = mkSystem $ \fo -> do
        t0 <- takeMVar (theTime fo)
        Just t <- GLFW.getTime
        threadDelay $ floor $ (t0 - t + dt) * 1000 * 1000
        putMVar (theTime fo) $ t0 + dt
    stand = mkSystem $ \fo -> takeMVar (theEnd fo)

newtype TextureStorage = TextureStorage { getTextureStorage :: IORef (IM.IntMap G.Texture) }

type DrawM = ReaderT (Location ()) IO

mapReaderWith :: (s -> r) -> (m a -> n b) -> ReaderT r m a -> ReaderT s n b
mapReaderWith f g m = unsafeCoerce $ \s -> g (unsafeCoerce m (f s))
{-# INLINE mapReaderWith #-}

instance Affine DrawM where
    translate v = mapReaderWith (translate v) (G.translate v)
    {-# INLINE translate #-}
    rotateD t = mapReaderWith (rotateD t) (G.rotateD t)
    {-# INLINE rotateD #-}
    rotateR t = let t' = t / pi * 180 in mapReaderWith (rotateR t) (G.rotateD t')
    {-# INLINE rotateR #-}
    scale v = mapReaderWith (scale v) (G.scale v)
    {-# INLINE scale #-}

instance (Given TextureStorage) => Picture2D DrawM where
    bitmap (Bitmap bmp h) = liftIO $ do
        m <- readIORef (getTextureStorage given)
        case IM.lookup h m of
            Just t -> G.drawTexture t
            Nothing -> do
                t <- G.installTexture bmp
                writeIORef (getTextureStorage given) $ IM.insert h t m
                G.drawTexture t
    bitmapOnce (Bitmap bmp _) = liftIO $ do
        t <- G.installTexture bmp
        G.drawTexture t
        G.releaseTexture t

    circle r = liftIO (G.circle r)
    {-# INLINE circle #-}
    circleOutline r = liftIO (G.circleOutline r)
    {-# INLINE circleOutline #-}
    polygon vs = liftIO (G.polygon vs)
    {-# INLINE polygon #-}
    polygonOutline vs = liftIO (G.polygonOutline vs)
    {-# INLINE polygonOutline #-}
    line vs = liftIO (G.line vs)
    {-# INLINE line #-}
    thickness t = mapReaderWith id (G.thickness t)
    {-# INLINE thickness #-}
    color c = mapReaderWith id (G.color c)
    {-# INLINE color #-}
    blendMode m = mapReaderWith id (G.blendMode m)
    {-# INLINE blendMode #-}

instance Local DrawM where
    getLocation = asks coerceLocation