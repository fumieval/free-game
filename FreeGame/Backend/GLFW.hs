{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
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
import Control.Monad.Free.Church
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Applicative
import Control.Artery
import Data.IORef
import Data.Reflection
import Data.BoundingBox
import qualified Data.Vector as V
import FreeGame.Class
import FreeGame.Component
import FreeGame.Data.Bitmap
import FreeGame.Types
import Linear
import qualified System.PortAudio as PA
-- #if (MIN_VERSION_containers(0,5,0))
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified FreeGame.Internal.GLFW as G
import qualified Graphics.UI.GLFW as GLFW
import Unsafe.Coerce
import Control.Concurrent
import Linear.V
import Control.Monad.Except
import GHC.Prim

runGame :: WindowMode -> BoundingBox2 -> System a -> IO ()
runGame win box m = do
    sys <- G.beginGLFW win box
    f <- Foundation
        <$> newMVar 0
        <*> pure 44100 -- FIX THIS
        <*> newMVar IM.empty
        <*> newMVar IM.empty
        <*> newMVar Nothing
        <*> newMVar IM.empty
        <*> newMVar IM.empty
        <*> newMVar 0
        <*> pure sys
        <*> newIORef IM.empty
        <*> newEmptyMVar
    let win = G.theWindow sys
    ref <- newEmptyMVar
    forkIO $ do
        s <- runSystem f m
        putMVar ref s
    r <- runExceptT $ PA.with undefined undefined undefined (audioProcess f) $ liftIO $ runGraphic f
    G.endGLFW sys
    return ()

data Foundation = Foundation
    { newComponentId :: MVar Int
    , sampleRate :: Double
    , cores :: MVar (IM.IntMap (Component Any System))
    , coreGraphic :: MVar (IM.IntMap Any) -- rely on `invoke`
    , coreAudio :: MVar (Maybe (Int, Any))
    , coreKeyboard :: MVar (IM.IntMap Any)
    , coreMouse :: MVar (IM.IntMap Any)
    , theTime :: MVar Double
    , theSystem :: G.System
    , textures :: IORef (IM.IntMap G.Texture)
    , theEnd :: MVar ()
    }
runSystem fo = iterM (runSys fo)

drawPicture :: forall a. Given TextureStorage => Picture a -> IO a
drawPicture (Picture m) = runReaderT (m :: DrawM a) (Location id id)

runGraphic :: Foundation -> IO ()
runGraphic fo = do
    js <- readMVar $ coreGraphic fo
    m <- takeMVar $ cores fo
    Just t <- GLFW.getTime
    print t
    G.beginFrame (theSystem fo)
    us <- forM (IM.assocs js) $ \(i, f) -> do
        (pic, c) <- runSystem fo $ runComponent (m IM.! i) (unsafeCoerce f t)
        give (TextureStorage (textures fo)) $ drawPicture pic
        return (i, c)
    putMVar (cores fo) $ foldr (uncurry IM.insert) m us
    b <- G.endFrame (theSystem fo)
    tryTakeMVar (theEnd fo) >>= \case
        Just _ -> return ()
        _ -> unless b (runGraphic fo)

v2v2 :: V2 Float -> V 2 Float
v2v2 (V2 x y) = case fromVector $ V.fromList [x, y] of
    Just a -> a

audioProcess :: Foundation -> Artery IO (PA.Chunk (V 0 Float)) [V 2 Float]
audioProcess fo = effectful $ \(PA.Chunk n _) -> readMVar (coreAudio fo) >>= \case
    Nothing -> return $ fmap v2v2 $ replicate n 0
    Just (j, pull) -> do
        m <- takeMVar $ cores fo
        Just t <- GLFW.getTime
        let Component f = m IM.! j
        (buf, c) <- runSystem fo $ f (unsafeCoerce pull (sampleRate fo * fromIntegral n) n)
        putMVar (cores fo) $ IM.insert j c m
        return (fmap v2v2 buf)

limitedMapMIntMap :: Monad m => IM.IntMap b -> (b -> a -> m a) -> IM.IntMap a -> m (IM.IntMap a)
limitedMapMIntMap (IM.assocs -> js) f m = liftM (foldr (uncurry IM.insert) m) $ forM js $ \(i, k) -> do
    a <- f k (m IM.! i)
    return (i, a)

sendEventIO :: Foundation -> e a -> Component e System -> IO (a, Component e System)
sendEventIO fo e = runSystem fo . flip runComponent e

keyCallback :: Foundation -> GLFW.KeyCallback
keyCallback fo _ k _ s _ = do
    js <- readMVar (coreKeyboard fo)
    modifyMVar_ (cores fo) $ limitedMapMIntMap js $ \m -> (fmap snd.) $ sendEventIO fo
        $ unsafeCoerce m (toEnum . fromEnum $ k :: Key) (GLFW.KeyState'Released /= s)

mouseButtonCallback :: Foundation -> GLFW.MouseButtonCallback
mouseButtonCallback fo _ btn s _ = do
    js <- readMVar (coreMouse fo)
    modifyMVar_ (cores fo) $ limitedMapMIntMap js $ \(unsafeCoerce -> (m, _, _)) -> (fmap snd.)
        $ sendEventIO fo $ m (fromEnum $ btn) (GLFW.MouseButtonState'Released /= s)

cursorPosCallback :: Foundation -> GLFW.CursorPosCallback
cursorPosCallback fo _ x y = do
    js <- readMVar (coreMouse fo)
    modifyMVar_ (cores fo) $ limitedMapMIntMap js $ \(unsafeCoerce -> (_, m, _)) -> (fmap snd.)
        $ sendEventIO fo $ m (V2 x y)

scrollCallback :: Foundation -> GLFW.ScrollCallback
scrollCallback fo _ x y = do
    js <- readMVar (coreMouse fo)
    modifyMVar_ (cores fo) $ limitedMapMIntMap js $ \(unsafeCoerce -> (_, _, m)) -> (fmap snd.)
        $ sendEventIO fo $ m (V2 x y)

assimilate :: Control e -> e a -> e a
assimilate _ = id

runSys :: Foundation -> Sys (IO a) -> IO a
runSys fo (LiftIO m) = join m
runSys fo (Command (Control i) e) = do
    m <- takeMVar $ cores fo
    (cont, c) <- runSystem fo $ runComponent (m IM.! i) (unsafeCoerce e)
    putMVar (cores fo) $ IM.insert i c m
    cont
runSys fo (Invoke c cont) = do
    n <- takeMVar $ newComponentId fo
    m <- takeMVar $ cores fo
    putMVar (cores fo) $ IM.insert n (unsafeCoerce c) m
    putMVar (newComponentId fo) (n + 1)
    cont $ Control n
runSys fo (ConnectGraphic con@(Control i) cont) = do
    modifyMVar_ (coreGraphic fo) $ return . IM.insert i (unsafeCoerce $ assimilate con . pullGraphic)
    cont
runSys fo (ConnectAudio con@(Control i) cont) = do
    _ <- swapMVar (coreAudio fo) $ Just (i, unsafeCoerce $ (assimilate con .) . pullAudio)
    cont
runSys fo (DisconnectGraphic (Control i) cont) = do
    modifyMVar_ (coreGraphic fo) $ return . IM.delete i
    cont
runSys fo (DisconnectAudio (Control i) cont) = do
    _ <- swapMVar (coreAudio fo) Nothing
    cont
runSys fo (ConnectKeyboard con@(Control i) cont) = do
    modifyMVar_ (coreKeyboard fo) $ return . IM.insert i (unsafeCoerce $ (assimilate con .) . keyEvent)
    cont
runSys fo (DisconnectKeyboard (Control i) cont) = do
    modifyMVar_ (coreKeyboard fo) $ return . IM.delete i
    cont
runSys fo (ConnectMouse con@(Control i) cont) = do
    modifyMVar_ (coreMouse fo) $ return . IM.insert i (unsafeCoerce
        ( (assimilate con .) . mouseButtonEvent
        , assimilate con . cursorEvent
        , assimilate con . scrollEvent))
    cont
runSys fo (DisconnectMouse (Control i) cont) = do
    modifyMVar_ (coreMouse fo) $ return . IM.delete i
    cont
runSys fo (Wait dt cont) = do
    t0 <- takeMVar (theTime fo)
    Just t <- GLFW.getTime
    threadDelay $ floor $ (t0 - t + dt) * 1000 * 1000
    putMVar (theTime fo) $ t0 + dt
    cont
runSys fo (Stand cont) = takeMVar (theEnd fo) >> cont

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