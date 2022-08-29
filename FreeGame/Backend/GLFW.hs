{-# LANGUAGE Rank2Types, BangPatterns, ViewPatterns, CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
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
module FreeGame.Backend.GLFW (Game(..), Frame, runGame) where
import Control.Lens (view)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Iter
import Control.Monad.Trans.Resource
import Data.BoundingBox
import Data.Functor.Identity
import Data.IORef
import FreeGame.Class
import FreeGame.Data.Bitmap
import FreeGame.Instances ()
import FreeGame.Types
import FreeGame.UI
import Linear
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as Map
import qualified FreeGame.Internal.GLFW as G
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.UI.GLFW as GLFW

keyCallback :: IORef (Map.Map Key ButtonState) -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallback keyBuffer _ key _ GLFW.KeyState'Pressed _ = modifyIORef' keyBuffer $ Map.adjust buttonDown (toEnum $ fromEnum key)
keyCallback keyBuffer _ key _ GLFW.KeyState'Released _ = modifyIORef' keyBuffer $ Map.adjust buttonUp (toEnum $ fromEnum key)
keyCallback _ _ _ _ _ _ = return ()

mouseButtonCallback :: IORef (Map.Map Int ButtonState) -> GLFW.Window -> GLFW.MouseButton -> GLFW.MouseButtonState -> GLFW.ModifierKeys -> IO ()
mouseButtonCallback mouseBuffer _ btn GLFW.MouseButtonState'Pressed _ = modifyIORef' mouseBuffer $ Map.adjust buttonDown (fromEnum btn)
mouseButtonCallback mouseBuffer _ btn GLFW.MouseButtonState'Released _ = modifyIORef' mouseBuffer $ Map.adjust buttonUp (fromEnum btn)

mouseEnterCallback :: IORef Bool -> GLFW.Window -> GLFW.CursorState -> IO ()
mouseEnterCallback ref _ GLFW.CursorState'InWindow = writeIORef ref True
mouseEnterCallback ref _ GLFW.CursorState'NotInWindow = writeIORef ref False

runGame :: WindowMode -> BoundingBox2 -> Game a -> IO (Maybe a)
runGame mode bbox m = G.withGLFW mode bbox (execGame m)

initialKeyBuffer :: Map.Map Key ButtonState
initialKeyBuffer = Map.fromList $ zip [minBound..] (repeat Release)

initialMouseBuffer :: Map.Map Int ButtonState
initialMouseBuffer = Map.fromList $ zip [0..7] (repeat Release)

execGame :: Game a -> G.System -> IO (Maybe a)
execGame m system = do
    textureStorage <- newIORef IM.empty
    keyBuffer <- newIORef initialKeyBuffer
    mouseBuffer <- newIORef initialMouseBuffer
    mouseIn <- newIORef True
    scroll <- newIORef (V2 0 0)
    GLFW.setKeyCallback (G.theWindow system) $ Just $ keyCallback keyBuffer
    GLFW.setMouseButtonCallback (G.theWindow system) $ Just $ mouseButtonCallback mouseBuffer
    GLFW.setCursorEnterCallback (G.theWindow system) $ Just $ mouseEnterCallback mouseIn
    GLFW.setScrollCallback (G.theWindow system) $ Just $ \_ x y -> modifyIORef scroll (+V2 x y)
    let drawLocation = Location id id
    runResourceT $ gameLoop Env{..} m

data Env = Env
    { system :: G.System
    , textureStorage :: IORef (IM.IntMap G.Texture)
    , keyBuffer :: IORef (Map.Map Key ButtonState)
    , mouseBuffer :: IORef (Map.Map Int ButtonState)
    , mouseIn :: IORef Bool
    , scroll :: IORef (V2 Double)
    , drawLocation :: Location ()
    }

newtype Game a = Game { unGame :: IterT Frame a }
    deriving (Functor, Applicative, Monad, MonadIO, Affine, Picture2D, Mouse, Keyboard, FreeGame, Local, MonadFree Identity)

instance MonadResource Game where
    liftResourceT = Game . lift . liftResourceT

newtype Frame a = Frame { unFrame :: ReaderT Env (ResourceT IO) a }
    deriving (Functor, Applicative, Monad, MonadResource, MonadIO, MonadReader Env)

gameLoop :: Env -> Game a -> ResourceT IO (Maybe a)
gameLoop env@Env{..} (Game m0) = flip fix m0 $ \self m -> do
    liftIO $ G.beginFrame system

    r <- runReaderT (unFrame (runIterT m)) env

    b <- liftIO $ do
        modifyIORef' keyBuffer (Map.map buttonStay)
        modifyIORef' mouseBuffer (Map.map buttonStay)
        writeIORef scroll (V2 0 0 :: V2 Double)
        G.endFrame system

    if b
        then return Nothing
        else either (return . Just) self r

withLocation :: (Location () -> Location ()) -> Env -> Env
withLocation f de = de { drawLocation = f $ drawLocation de }

instance FreeGame Frame where
    preloadBitmap (Bitmap bmp h) = Frame $ ReaderT $ \Env{..} -> do
        m <- liftIO $ readIORef textureStorage
        case IM.lookup h m of
            Just _ -> return ()
            Nothing -> do
                t <- liftIO $ G.installTexture bmp
                liftIO $ writeIORef textureStorage $ IM.insert h t m
                void $ register $ G.releaseTexture t >> modifyIORef' textureStorage (IM.delete h)

    takeScreenshot = Frame $ ReaderT $ \Env{..} -> liftIO (G.screenshot system >>= liftBitmapIO)
    clearColor (V4 r g b a) = liftIO $ GL.clearColor GL.$= GL.Color4 r g b a
    setTitle str = Frame $ ReaderT $ \Env{..} -> liftIO $ GLFW.setWindowTitle (G.theWindow system) str
    showCursor = Frame $ ReaderT $ \Env{..} -> liftIO $ GLFW.setCursorInputMode (G.theWindow system) GLFW.CursorInputMode'Normal
    hideCursor = Frame $ ReaderT $ \Env{..} -> liftIO $ GLFW.setCursorInputMode (G.theWindow system) GLFW.CursorInputMode'Hidden
    setFPS n = Frame $ ReaderT $ \Env{..} -> liftIO $ writeIORef (G.theFPS system) n
    getFPS = Frame $ ReaderT $ \Env{..} -> liftIO (readIORef (G.currentFPS system))
    getBoundingBox = Frame $ ReaderT $ \Env{..} -> liftIO (readIORef (G.refRegion system))
    setBoundingBox bbox@(view (size zero)-> V2 w h) = Frame $ ReaderT $ \Env{..} -> do
        liftIO $ GLFW.setWindowSize (G.theWindow system) (floor w) (floor h)
        liftIO $ writeIORef (G.refRegion system) bbox

instance Keyboard Frame where
    keyStates_ = Frame $ ReaderT $ \Env{..} -> liftIO (readIORef keyBuffer)

instance Mouse Frame where
    mouseButtons_ = Frame $ ReaderT $ \Env{..} -> liftIO (readIORef mouseBuffer)
    globalMousePosition = Frame $ ReaderT $ \Env{..} -> do
        (x, y) <- liftIO $ GLFW.getCursorPos (G.theWindow system)
        pure $ V2 x y
    mouseScroll = Frame $ ReaderT $ \Env{..} -> liftIO (readIORef scroll)
    mouseInWindow = Frame $ ReaderT $ \Env{..} -> liftIO (readIORef mouseIn)

mapReaderWith :: (Env -> Env) -> (IO a -> IO a) -> Frame a -> Frame a
mapReaderWith f g m = Frame $ ReaderT $ \s -> transResourceT g (runReaderT (unFrame m) (f s))
{-# INLINE mapReaderWith #-}

instance Affine Frame where
    translate v = mapReaderWith (withLocation $ translate v) (G.translate v)
    {-# INLINE translate #-}
    rotateD t = mapReaderWith (withLocation $ rotateD t) (G.rotateD t)
    {-# INLINE rotateD #-}
    rotateR t = let t' = t / pi * 180 in mapReaderWith (withLocation $ rotateR t) (G.rotateD t')
    {-# INLINE rotateR #-}
    scale v = mapReaderWith (withLocation $ scale v) (G.scale v)
    {-# INLINE scale #-}

instance Picture2D Frame where
    bitmap (Bitmap bmp h) = Frame $ ReaderT $ \Env{..} -> do
        m <- liftIO $ readIORef textureStorage
        case IM.lookup h m of
            Just t -> liftIO $ G.drawTexture t
            Nothing -> do
                t <- liftIO $ G.installTexture bmp
                liftIO $ writeIORef textureStorage $ IM.insert h t m
                _ <- register $ modifyIORef' textureStorage $ IM.delete h
                liftIO $ G.drawTexture t
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

instance Local Frame where
    getLocation = asks $ coerceLocation . drawLocation
