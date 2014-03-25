{-# LANGUAGE Rank2Types, BangPatterns, ViewPatterns #-}
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
import Data.BoundingBox.Dim2
import FreeGame.Class
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
import Codec.Picture.RGBA8
import Control.Concurrent
import Control.Lens (view)

keyCallback :: IORef (Map.Map Key ButtonState) -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallback keyBuffer _ key _ GLFW.KeyState'Pressed _ = modifyIORef' keyBuffer $ Map.adjust buttonDown (toEnum $ fromEnum key)
keyCallback keyBuffer _ key _ GLFW.KeyState'Released _ = modifyIORef' keyBuffer $ Map.adjust buttonUp (toEnum $ fromEnum key)
keyCallback _ _ _ _ _ _ = return ()

mouseButtonCallback :: IORef (Map.Map Int ButtonState) -> GLFW.Window -> GLFW.MouseButton -> GLFW.MouseButtonState -> GLFW.ModifierKeys -> IO ()
mouseButtonCallback mouseBuffer _ btn GLFW.MouseButtonState'Pressed _ = modifyIORef' mouseBuffer $ Map.adjust buttonDown (fromEnum btn)
mouseButtonCallback mouseBuffer _ btn GLFW.MouseButtonState'Released _ = modifyIORef' mouseBuffer $ Map.adjust buttonUp (fromEnum btn)

runGame :: WindowMode -> BoundingBox Double -> IterT (F UI) a -> IO (Maybe a)
runGame mode bbox m = G.withGLFW mode bbox (execGame m)

initialKeyBuffer :: Map.Map Key ButtonState
initialKeyBuffer = Map.fromList $ zip [minBound..] (repeat Release)

initialMouseBuffer :: Map.Map Int ButtonState
initialMouseBuffer = Map.fromList $ zip [0..7] (repeat Release)

execGame :: IterT (F UI) a -> G.System -> IO (Maybe a)
execGame m sys = do
    texs <- newIORef IM.empty
    keyBuffer <- newIORef initialKeyBuffer
    mouseBuffer <- newIORef initialMouseBuffer
    GLFW.setKeyCallback (G.theWindow sys) $ Just $ keyCallback keyBuffer
    GLFW.setMouseButtonCallback (G.theWindow sys) $ Just $ mouseButtonCallback mouseBuffer

    execFinalizerT
        $ give (RefKeyStates keyBuffer)
        $ give (RefMouseButtonStates mouseBuffer)
        $ give (TextureStorage texs)
        $ give sys
        $ gameLoop m

gameLoop ::
    ( Given G.System
    , Given TextureStorage
    , Given KeyStates
    , Given MouseButtonStates
    ) => IterT (F UI) a -> FinalizerT IO (Maybe a)
gameLoop m = do
    liftIO $ G.beginFrame given

    fs <- liftIO $ newIORef ([] :: [IO ()])

    r <- give fs $ iterM runUI $ runIterT m

    b <- liftIO $ do
        modifyIORef' (getKeyStates given) (Map.map buttonStay)
        modifyIORef' (getMouseButtonStates given) (Map.map buttonStay)
        G.endFrame given
    liftIO (readIORef fs) >>= finalizer . sequence_

    if b
        then return Nothing
        else either (return . Just) gameLoop r

newtype TextureStorage = TextureStorage { getTextureStorage :: IORef (IM.IntMap G.Texture) }

type DrawM = ReaderT (Location ()) IO

newtype KeyStates = RefKeyStates { getKeyStates :: IORef (Map.Map Key ButtonState) }
newtype MouseButtonStates = RefMouseButtonStates { getMouseButtonStates :: IORef (Map.Map Int ButtonState) }

runUI :: forall a.
    ( Given G.System
    , Given TextureStorage
    , Given KeyStates
    , Given MouseButtonStates
    , Given (IORef [IO ()])
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
    t <- liftIO $ G.installTexture bmp
    h <- liftIO $ addrImage bmp
    liftIO $ modifyIORef' (getTextureStorage given) (IM.insert h t)
    finalizer $ G.releaseTexture t >> modifyIORef' (getTextureStorage given) (IM.delete h)
    cont
runUI (KeyStates cont) = liftIO (readIORef $ getKeyStates given) >>= cont
runUI (MouseButtons cont) = liftIO (readIORef $ getMouseButtonStates given) >>= cont
-- runUI _ _ (MouseWheel cont) = GLFW.getMouseWheel >>= cont
runUI (MousePosition cont) = do
    (x, y) <- liftIO $ GLFW.getCursorPos (G.theWindow given)
    cont $ V2 x y
runUI (Bracket m) = join $ iterM runUI m
runUI (TakeScreenshot cont) = liftIO (G.screenshot given) >>= cont
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
runUI (ForkFrame m cont) = do
    _ <- liftIO $ forkIO $ do
        (_, f) <- runFinalizerT $ iterM runUI m
        modifyIORef' given (f:)
    cont
runUI (GetBoundingBox cont) = liftIO (readIORef (G.refRegion given)) >>= cont
runUI (SetBoundingBox bbox@(view (size C)-> V2 w h) cont) = do
    liftIO $ GLFW.setWindowSize (G.theWindow given) (floor w) (floor h)
    liftIO $ writeIORef (G.refRegion given) bbox
    cont


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

newtype Context = Context { getContext :: IORef [(G.Texture, Int)] }

instance (Given Context, Given TextureStorage) => Picture2D DrawM where
    bitmap bmp = liftIO $ do
        m <- readIORef (getTextureStorage given)
        h <- addrImage bmp
        case IM.lookup h m of
            Just t -> G.drawTexture t
            Nothing -> do
                t <- G.installTexture bmp
                writeIORef (getTextureStorage given) $ IM.insert h t m
                modifyIORef (getContext given) ((t, h) :)
                G.drawTexture t
    bitmapOnce bmp = liftIO $ do
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
