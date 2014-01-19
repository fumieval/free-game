{-# LANGUAGE FlexibleInstances, FlexibleContexts, Rank2Types, BangPatterns #-}
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
import Data.Maybe
import FreeGame.Class
import FreeGame.Data.Bitmap
import FreeGame.Data.Wave
import FreeGame.Internal.Finalizer
import FreeGame.UI
import FreeGame.Types
import Linear
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as Map
import qualified FreeGame.Internal.GLFW as G
import qualified Graphics.UI.GLFW as GLFW
import DSP.Artery.IO
import Data.Function

runGame :: IterT (F UI) a -> IO (Maybe a)
runGame m_ = G.withGLFW 60 (BoundingBox 0 0 640 480)
    $ \sys -> do
        texs <- newIORef IM.empty
        str <- Stream <$> newMVar []
        keyBuffer <- newIORef initialKeyBuffer
        mouseBuffer <- newIORef initialMouseBuffer
        keyBuffer' <- newIORef initialKeyBuffer
        mouseBuffer' <- newIORef initialMouseBuffer 
        GLFW.setKeyCallback (G.theWindow sys) $ Just
            $ \_ key _ st _ -> print (key, toEnum $ fromEnum key :: Key) >> modifyIORef' keyBuffer (Map.insert (toEnum $ fromEnum key) (G.fromKeyState st))
        GLFW.setMouseButtonCallback (G.theWindow sys) $ Just
            $ \_ btn st _ -> print btn >> modifyIORef' mouseBuffer (Map.insert (fromEnum btn) (G.fromMouseButtonState st))
        -- setScrollCallback (G.theWindow sys)
        -- withStream def (const $ streamTap str) $ 

        execFinalizerT
            $ give (RefKeyStates keyBuffer)
            $ give (RefMouseButtonStates mouseBuffer)
            $ give (Previous (RefKeyStates keyBuffer'))
            $ give (Previous (RefMouseButtonStates mouseBuffer'))
            $ give str
            $ give (TextureStorage texs)
            $ give sys
            $ go m_
    where
        !initialKeyBuffer = Map.fromList $ zip [minBound..] (repeat False)
        !initialMouseBuffer = Map.fromList $ zip [0..7] (repeat False)
        go :: (Given Stream
            , Given G.System
            , Given TextureStorage
            , Given KeyStates
            , Given MouseButtonStates
            , Given (Previous KeyStates)
            , Given (Previous MouseButtonStates)
            ) => IterT (F UI) a -> FinalizerT IO (Maybe a)
        go m = do
            liftIO $ G.beginFrame given

            r <- iterM runUI $ runIterT m

            b <- liftIO $ do
                readIORef (getKeyStates given) >>= writeIORef (getKeyStates (getPrevious given))
                readIORef (getMouseButtonStates given) >>= writeIORef (getMouseButtonStates (getPrevious given))
                writeIORef (getKeyStates given) initialKeyBuffer
                writeIORef (getMouseButtonStates given) initialMouseBuffer
                G.endFrame given

            if b
                then return Nothing
                else case r of
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
    -- Just t0 <- liftIO GLFW.getTime
    cxt <- liftIO $ newIORef []
    cont <- liftIO $ give (Context cxt) $ runReaderT (m :: DrawM (FinalizerT IO a)) (Location id id)
    xs <- liftIO (readIORef cxt)
    forM xs $ \(t, h) -> finalizer $ G.releaseTexture t >> modifyIORef' (getTextureStorage given) (IM.delete h)
    -- Just t <- liftIO GLFW.getTime
    cont

runUI (FromFinalizer m) = join m
runUI (Configure _ cont) = cont
runUI (PreloadBitmap bmp cont) = do
    _ <- liftIO $ loadTexture given bmp
    cont
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
runUI (TakeScreenshot cont) = liftIO (G.screenshot given >>= makeStableBitmap) >>= cont

mapReaderWith :: (s -> r) -> (m a -> n b) -> ReaderT r m a -> ReaderT s n b
mapReaderWith f g m = ReaderT $ \s -> g (runReaderT m (f s))
{-# INLINE mapReaderWith #-}

instance Affine DrawM where
    translate v = mapReaderWith (translate v) (G.translate v)
    {-# INLINE translate #-}
    rotateD t = mapReaderWith (rotateD t) (G.rotateD t)
    rotateR t = let t' = t * pi / 180 in mapReaderWith (rotateD t') (G.rotateD t')
    scale v = mapReaderWith (scale v) (G.scale v)

loadTexture :: TextureStorage -> Bitmap -> IO (G.Texture, Maybe Int)
loadTexture (TextureStorage st) bmp@(BitmapData _ (Just h)) = do
    m <- readIORef st
    case IM.lookup h m of
        Just t -> return (t, Nothing)
        Nothing -> do
            t <- G.installTexture bmp
            writeIORef st $ IM.insert h t m
            return (t, Just h)
loadTexture _ _ = undefined

newtype Context = Context { getContext :: IORef [(G.Texture, Int)] }

instance (Given Context, Given TextureStorage) => Picture2D DrawM where
    bitmap bmp@(BitmapData _ Nothing) = liftIO $ do
        t <- G.installTexture bmp
        G.drawTexture t
        G.releaseTexture t
    bitmap bmp = liftIO $ do
        (t, h) <- loadTexture given bmp
        case h of
            Just i -> modifyIORef (getContext given) ((t, i) :)
            Nothing -> return ()
        G.drawTexture t
    {-# INLINE bitmap #-}

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

instance Local DrawM where
    getLocation = asks coerceLocation
