{-# LANGUAGE Rank2Types, BangPatterns #-}
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
import Control.Monad.Free.Church
import Control.Monad.Trans.Iter
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.IORef
import Data.Reflection
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

keyCallback :: IORef (Map.Map Key Bool) -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallback keyBuffer _ key _ GLFW.KeyState'Pressed _ = modifyIORef' keyBuffer $ Map.insert (toEnum $ fromEnum key) True
keyCallback _ _ _ _ _ _ = return ()

mouseButtonCallback :: IORef (Map.Map Int Bool) -> GLFW.Window -> GLFW.MouseButton -> GLFW.MouseButtonState -> GLFW.ModifierKeys -> IO ()
mouseButtonCallback mouseBuffer _ btn GLFW.MouseButtonState'Pressed _ = modifyIORef' mouseBuffer (Map.insert (fromEnum btn) True)
mouseButtonCallback _ _ _ _ _ = return ()

runGame :: WindowMode -> BoundingBox Double -> IterT (F UI) a -> IO (Maybe a)
runGame mode bbox m = G.withGLFW mode bbox (execGame m)

initialKeyBuffer :: Map.Map Key Bool
initialKeyBuffer = Map.fromList $ zip [minBound..] (repeat False)

initialMouseBuffer :: Map.Map Int Bool
initialMouseBuffer = Map.fromList $ zip [0..7] (repeat False)

execGame :: IterT (F UI) a -> G.System -> IO (Maybe a)
execGame m sys = do
    texs <- newIORef IM.empty
    keyBuffer <- newIORef initialKeyBuffer
    mouseBuffer <- newIORef initialMouseBuffer
    keyBuffer' <- newIORef initialKeyBuffer
    mouseBuffer' <- newIORef initialMouseBuffer 
    GLFW.setKeyCallback (G.theWindow sys) $ Just $ keyCallback keyBuffer
    GLFW.setMouseButtonCallback (G.theWindow sys) $ Just $ mouseButtonCallback mouseBuffer

    execFinalizerT
        $ give (RefKeyStates keyBuffer)
        $ give (RefMouseButtonStates mouseBuffer)
        $ give (Previous (RefKeyStates keyBuffer'))
        $ give (Previous (RefMouseButtonStates mouseBuffer'))
        $ give (TextureStorage texs)
        $ give sys
        $ gameLoop m

gameLoop ::
    ( Given G.System
    , Given TextureStorage
    , Given KeyStates
    , Given MouseButtonStates
    , Given (Previous KeyStates)
    , Given (Previous MouseButtonStates)
    ) => IterT (F UI) a -> FinalizerT IO (Maybe a)
gameLoop m = do
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
            Iter cont -> gameLoop cont
            Pure a -> return (Just a)    

newtype TextureStorage = TextureStorage { getTextureStorage :: IORef (IM.IntMap G.Texture) }

type DrawM = ReaderT (Location ()) IO

newtype Previous a = Previous { getPrevious :: a }

newtype KeyStates = RefKeyStates { getKeyStates :: IORef (Map.Map Key Bool) }
newtype MouseButtonStates = RefMouseButtonStates { getMouseButtonStates :: IORef (Map.Map Int Bool) }


runUI :: forall a.
    ( Given G.System
    , Given TextureStorage
    , Given KeyStates
    , Given MouseButtonStates
    , Given (Previous KeyStates)
    , Given (Previous MouseButtonStates)
    ) => UI (FinalizerT IO a) -> FinalizerT IO a
runUI (Draw m) = do
    (cont, xs) <- liftIO $ do
        cxt <- newIORef []
        cont <- give (Context cxt) $ runReaderT (m :: DrawM (FinalizerT IO a)) (Location id id)
        xs <- readIORef cxt
        return (cont, xs)
    unless (null xs) $ finalizer $ forM_ xs $ \(t, h) -> G.releaseTexture t >> modifyIORef' (getTextureStorage given) (IM.delete h)
    cont
runUI (FromFinalizer m) = join m
runUI (PreloadBitmap bmp cont) = do
    loadTexture given bmp
        (\t h -> finalizer $ G.releaseTexture t >> modifyIORef' (getTextureStorage given) (IM.delete h))
        (const $ return ())
        (const $ return ())
    cont
runUI (KeyStates cont) = do
    let k = liftIO . readIORef . getKeyStates
    s <- k given
    t <- k (getPrevious given)
    cont s t
runUI (MouseButtons cont) = do
    let k = liftIO . readIORef . getMouseButtonStates
    s <- k given
    t <- k (getPrevious given)
    cont s t
-- runUI _ _ (MouseWheel cont) = GLFW.getMouseWheel >>= cont
runUI (MousePosition cont) = do
    (x, y) <- liftIO $ GLFW.getCursorPos (G.theWindow given)
    cont $ V2 x y
runUI (Bracket m) = join $ iterM runUI m
runUI (TakeScreenshot cont) = liftIO (G.screenshot given >>= makeStableBitmap) >>= cont
runUI (ClearColor col cont) = do
    liftIO $ GL.clearColor GL.$= unsafeCoerce col
    cont
runUI (SetTitle str cont) = do
    liftIO $ GLFW.setWindowTitle (G.theWindow given) str
    cont
runUI (ShowCursor cont) = do
    liftIO $ GLFW.setCursorInputMode (G.theWindow given) GLFW.CursorInputMode'Normal
    cont
runUI (HideCursor cont) = do
    liftIO $ GLFW.setCursorInputMode (G.theWindow given) GLFW.CursorInputMode'Hidden
    cont
runUI (SetFPS n cont) = do
    liftIO $ writeIORef (G.theFPS given) n
    cont
runUI (GetFPS cont) = liftIO (readIORef (G.currentFPS given)) >>= cont

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

loadTexture :: MonadIO m => TextureStorage -> Bitmap
    -> (G.Texture -> Int -> m ())
    -> (G.Texture -> m ())
    -> (G.Texture -> m ())
    -> m ()
loadTexture (TextureStorage st) (BitmapData img (Just h)) hook cont _ = do
    m <- liftIO $ readIORef st
    case IM.lookup h m of
        Just t -> cont t
        Nothing -> do
            t <- liftIO $ G.installTexture img
            liftIO $ writeIORef st $ IM.insert h t m
            hook t h
            cont t
loadTexture _ (BitmapData img Nothing) _ cont fin = do
    t <- liftIO $ G.installTexture img
    cont t
    fin t

newtype Context = Context { getContext :: IORef [(G.Texture, Int)] }

instance (Given Context, Given TextureStorage) => Picture2D DrawM where
    bitmap bmp = liftIO $ loadTexture given bmp
        (\t h -> modifyIORef (getContext given) ((t, h) :))
        G.drawTexture
        G.releaseTexture
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
