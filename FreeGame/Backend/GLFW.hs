-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.FreeGame.GUI.GLFW
-- Copyright   :  (C) 2013 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Graphics.UI.FreeGame.GUI.GLFW (runGame) where
import Control.Applicative
import Control.Monad
import Control.Monad.Free.Church
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.FreeGame.Base
import Graphics.UI.FreeGame.Data.Bitmap
import Graphics.UI.FreeGame.GUI
import System.IO.Unsafe
import Control.Concurrent.MVar
import qualified Graphics.UI.FreeGame.Internal.GLFW as G

runGame :: GUIParam -> F GUI a -> IO (Maybe a)
runGame (GUIParam _fps title window cur col (BoundingBox x0 y0 x1 y1)) m = G.withGLFW
    title
    window
    cur
    (floor $ x1 - x0)
    (floor $ y1 - y0)
    col
    $ runFinalizerT $ unUI m runGUI tickIO (return . Just)

newtype TextureStorage = TextureStorage { getTextureStorage :: IORef (IM.IntMap (GL.TextureObject, Double, Double)) }

mapReaderWith :: (s -> r) -> (m a -> n b) -> ReaderT r m a -> ReaderT s n b
mapReaderWith f g m = ReaderT $ \s -> g (runReaderT m (f s))

instance Affine (ReaderT (ViewPort ()) IO) where
    translate v = mapReaderWith (translate v) (G.translate v)
    rotateD t = mapReaderWith (rotateD t) (G.rotateD t)
    rotateR t = mapReaderWith (rotateR t) (G.rotateR t)
    scale v = mapReaderWith (scale v) (G.scale v)

instance Given TextureStorage => Picture2D (ReaderT (ViewPort ()) IO) where
    bitmap bmp@(BitmapData _ (Just h)) = do
        m <- liftIO $ readIORef (refTextures given)
        case IM.lookup h m of
            Just t -> liftIO $ drawTexture t
            Nothing -> do
                t <- installTexture bmp
                liftIO $ writeIORef (refTextures given) $ IM.insert h t m
                liftIO $ drawTexture t
                finalizer $ modifyIORef (refTextures given) $ IM.delete h
    bitmap bmp@(BitmapData _ Nothing) = liftIO $ runFinalizerT $ do
        t <- installTexture bmp
        liftIO $ drawTexture t
    circle r = liftIO (G.circle r)
    circleOutline r = liftIO (G.circleOutline r)
    polygon vs = liftIO (G.polygon vs)
    polygonOutline vs = liftIO (G.polygonOutline vs)
    line vs = liftIO (G.line vs)
    thickness t = mapReaderWith id (G.thickness t)
    colored = mapReaderWith id (G.colored t)

instance Local (ReaderT (ViewPort ()) IO) where
    getViewPort = asks coerceViewPort

data Stream = Stream [V2 Float]

streamTap :: MVar Stream -> Artery IO () (V2 CFloat)
streamTap mstr = effectful $ const $ do
    Stream st <- takeMVar mstr
    case mstr of
        [] -> putMVar (Stream st) >> return zero
        (x:xs) -> putMVar (Stream xs) >> return (fmap unsafeCoerce x)

runGUI :: MVar Stream -> GUI (FinalizerT IO a) -> FinalizerT IO a
runGUI _ (KeyState j cont) = GLFW.getKey (mapKey k) >>= cont
runGUI _ (MouseButtonL cont) = GLFW.mouseButtonIsPressed GLFW.MouseButton0 >>= cont
runGUI _ (MouseButtonR cont = GLFW.mouseButtonIsPressed GLFW.MouseButton1 >>= cont
runGUI _ (MouseButtonM cont) = GLFW.mouseButtonIsPressed GLFW.MouseButton2 >>= cont
runGUI _ (MouseWheel cont) = GLFW.getMouseWheel >>= cont
runGUI _ (MousePosition cont) = do
    (x, y) <- GLFW.getMousePosition
    cont $ V2 (fromIntegral x) (fromIntegral y)
runGUI mstr (Play (Wave w _) cont) = do
    liftIO $ takeMVar mstr >>= \(Stream st) -> putar mstr $ Stream (go st w)
    cont
    where
        go (x:xs) (y:ys) = x + y : go xs ys
        go xs [] = xs
        go [] ys = ys