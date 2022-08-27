{-# LANGUAGE Rank2Types, BangPatterns, ViewPatterns, CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
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
import Control.Monad.Free.Church
import Control.Monad.Trans.Iter
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.IORef
import Data.BoundingBox
import FreeGame.Class
import FreeGame.Data.Bitmap
import FreeGame.Internal.Finalizer
import FreeGame.UI
import FreeGame.Types
import Linear
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as Map
import qualified FreeGame.Internal.GLFW as G
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL.GL as GL
import Unsafe.Coerce
import Control.Lens (view)

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

runGame :: WindowMode -> BoundingBox2 -> IterT (F UI) a -> IO (Maybe a)
runGame mode bbox m = G.withGLFW mode bbox (execGame m)

initialKeyBuffer :: Map.Map Key ButtonState
initialKeyBuffer = Map.fromList $ zip [minBound..] (repeat Release)

initialMouseBuffer :: Map.Map Int ButtonState
initialMouseBuffer = Map.fromList $ zip [0..7] (repeat Release)

execGame :: IterT (F UI) a -> G.System -> IO (Maybe a)
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
    execFinalizerT $ gameLoop Env{..} m

data Env = Env
    { system :: G.System
    , textureStorage :: IORef (IM.IntMap G.Texture)
    , keyBuffer :: IORef (Map.Map Key ButtonState)
    , mouseBuffer :: IORef (Map.Map Int ButtonState)
    , mouseIn :: IORef Bool
    , scroll :: IORef (V2 Double)
    }

gameLoop :: Env -> IterT (F UI) a -> FinalizerT IO (Maybe a)
gameLoop env@Env{..} = fix $ \self m -> do
    liftIO $ G.beginFrame system

    r <- iterM (runUI env) $ runIterT m

    b <- liftIO $ do
        modifyIORef' keyBuffer (Map.map buttonStay)
        modifyIORef' mouseBuffer (Map.map buttonStay)
        writeIORef scroll (V2 0 0 :: V2 Double)
        G.endFrame system

    if b
        then return Nothing
        else either (return . Just) self r

data DrawEnv = DrawEnv
    { deTextureStorage :: IORef (IM.IntMap G.Texture)
    , deContext :: IORef [(G.Texture, Int)]
    , deLocation :: Location ()
    }

withLocation :: (Location () -> Location ()) -> DrawEnv -> DrawEnv
withLocation f de = de { deLocation = f $ deLocation de }

type DrawM = ReaderT DrawEnv IO

runUI :: forall a. Env -> UI (FinalizerT IO a) -> FinalizerT IO a
runUI Env{..} = \case
    Draw m -> do
        (cont, xs) <- liftIO $ do
            cxt <- newIORef []
            cont <- runReaderT (m :: DrawM (FinalizerT IO a))
                DrawEnv { deLocation = Location id id, deContext = cxt, deTextureStorage = textureStorage }
            xs <- readIORef cxt
            return (cont, xs)
        unless (null xs) $ finalizer $ forM_ xs $ \(t, h) -> G.releaseTexture t >> modifyIORef' textureStorage (IM.delete h)
        cont
    FromFinalizer m -> join m
    PreloadBitmap (Bitmap bmp h) cont -> do
        m <- liftIO $ readIORef textureStorage
        case IM.lookup h m of
            Just _ -> return ()
            Nothing -> do
                t <- liftIO $ G.installTexture bmp
                liftIO $ writeIORef textureStorage $ IM.insert h t m
                finalizer $ G.releaseTexture t >> modifyIORef' textureStorage (IM.delete h)
        cont
    KeyStates cont -> liftIO (readIORef keyBuffer) >>= cont
    MouseButtons cont -> liftIO (readIORef mouseBuffer) >>= cont
    MousePosition cont -> do
        (x, y) <- liftIO $ GLFW.getCursorPos (G.theWindow system)
        cont $ V2 x y
    MouseScroll cont -> liftIO (readIORef scroll) >>= cont
    MouseInWindow cont -> liftIO (readIORef mouseIn) >>= cont
    TakeScreenshot cont -> liftIO (G.screenshot system >>= liftBitmapIO) >>= cont
    ClearColor col cont -> do
        liftIO $ GL.clearColor GL.$= unsafeCoerce col
        cont
    SetTitle str cont -> do
        liftIO $ GLFW.setWindowTitle (G.theWindow system) str
        cont
    ShowCursor cont -> do
        liftIO $ GLFW.setCursorInputMode (G.theWindow system) GLFW.CursorInputMode'Normal
        cont
    HideCursor cont -> do
        liftIO $ GLFW.setCursorInputMode (G.theWindow system) GLFW.CursorInputMode'Hidden
        cont
    SetFPS n cont -> do
        liftIO $ writeIORef (G.theFPS system) n
        cont
    GetFPS cont -> liftIO (readIORef (G.currentFPS system)) >>= cont
    GetBoundingBox cont -> liftIO (readIORef (G.refRegion system)) >>= cont
    SetBoundingBox bbox@(view (size zero)-> V2 w h) cont -> do
        liftIO $ GLFW.setWindowSize (G.theWindow system) (floor w) (floor h)
        liftIO $ writeIORef (G.refRegion system) bbox
        cont

mapReaderWith :: (s -> r) -> (m a -> n b) -> ReaderT r m a -> ReaderT s n b
mapReaderWith f g m = ReaderT $ \s -> g (runReaderT m (f s))
{-# INLINE mapReaderWith #-}

instance Affine DrawM where
    translate v = mapReaderWith (withLocation $ translate v) (G.translate v)
    {-# INLINE translate #-}
    rotateD t = mapReaderWith (withLocation $ rotateD t) (G.rotateD t)
    {-# INLINE rotateD #-}
    rotateR t = let t' = t / pi * 180 in mapReaderWith (withLocation $ rotateR t) (G.rotateD t')
    {-# INLINE rotateR #-}
    scale v = mapReaderWith (withLocation $ scale v) (G.scale v)
    {-# INLINE scale #-}

instance Picture2D DrawM where
    bitmap (Bitmap bmp h) = ReaderT $ \DrawEnv{..} -> do
        m <- readIORef deTextureStorage
        case IM.lookup h m of
            Just t -> G.drawTexture t
            Nothing -> do
                t <- G.installTexture bmp
                writeIORef deTextureStorage $ IM.insert h t m
                modifyIORef deContext ((t, h) :)
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
    getLocation = asks $ coerceLocation . deLocation
