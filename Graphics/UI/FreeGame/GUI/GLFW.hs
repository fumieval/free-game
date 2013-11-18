{-# LANGUAGE ImplicitParams, BangPatterns #-}
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
module Graphics.UI.FreeGame.GUI.GLFW (runGame
    -- * Implementation details
    , Texture
    , installTexture
    , drawTexture
    , drawTextureAt) where
import Control.Applicative
import Control.Monad
import Control.Monad.Free.Church
import Control.Monad.IO.Class
import Data.IORef
import Data.Color
import Data.Char
import Foreign.ForeignPtr
import Graphics.UI.FreeGame.Base
import Graphics.UI.FreeGame.Data.Bitmap
import Graphics.UI.FreeGame.GUI
import qualified Data.Array.Repa.Repr.ForeignPtr as RF

import qualified Data.IntMap as IM
import System.Mem

import Control.Bool
import Linear

import qualified Graphics.UI.FreeGame.Internal.GLFW as G

data Window = Window
    { _windowBoundingBox :: BoundingBox Float
    , _windowMain :: F GUI ()
    , _windowTitle :: String
    , _windowResizable :: Bool
    }

runGame :: GUIParam -> F GUI a -> IO (Maybe a)
runGame param m = launch param $ unUI m runGUI tickIO (return . Just)

newtype TextureStorage = TextureStorage { getTextureStorage :: IORef (IM.IntMap (GL.TextureObject, Double, Double)) }

instance Affine IO where
    translate = G.translate
    rotateD = G.rotateD
    scale = G.scale
    colored = G.colored

instance Given TextureStorage => Picture2D IO where
    fromBitmap bmp@(BitmapData _ (Just h)) cont) = do
        m <- liftIO $ readIORef (refTextures given)
        case IM.lookup h m of
            Just t -> liftIO $ drawTexture t
            Nothing -> do
                t <- installTexture bmp
                liftIO $ writeIORef (refTextures given) $ IM.insert h t m
                liftIO $ drawTexture t
                finalizer $ modifyIORef (refTextures given) $ IM.delete h
    fromBitmap bmp@(BitmapData _ Nothing) cont) = do
        liftIO $ runFinalizerT $ installTexture bmp >>= liftIO . drawTexture

    circle = G.circle
    circleOutline = G.circleOutline
    polygon = G.polygon
    polygonOutline G.polygonOutline
    line = G.line
    thickness = G.thickness

runGUI (CharKey ch cont) = GLFW.getKey (mapCharKey ch) >>= cont
runGUI (SpecialKey k cont) = GLFW.getKey (mapSpecialKey k) >>= cont
runGUI (MouseButtonL cont) = GLFW.mouseButtonIsPressed GLFW.MouseButton0 >>= cont
runGUI (MouseButtonR cont = GLFW.mouseButtonIsPressed GLFW.MouseButton1 >>= cont
runGUI (MouseButtonM cont) = GLFW.mouseButtonIsPressed GLFW.MouseButton2 >>= cont
runGUI (MouseWheel cont) = GLFW.getMouseWheel >>= cont
runGUI (MousePosition cont) = do
    (x, y) <- GLFW.getMousePosition
    V2 (fromIntegral x) (fromIntegral y) >>= cont

type Texture = (GL.TextureObject, Double, Double)

launch :: GUIParam -> FinalizerT IO (Maybe a) -> IO (Maybe a)
launch param m = do
    () <- unlessM GLFW.init (fail "Failed to initialize")
    pf <- GLFW.openGLProfile
    let V2 ww wh = _windowSize param
    createWindow  GLFW.openWindow $ GLFW.defaultDisplayOptions {
        GLFW.displayOptions_width = ww
        ,GLFW.displayOptions_height = wh
        ,GLFW.displayOptions_displayMode = if _windowed param then GLFW.Window else GLFW.Fullscreen
        ,GLFW.displayOptions_windowIsResizable = False
        ,GLFW.displayOptions_openGLProfile = pf
        }

    GLFW.setWindowTitle $ _windowTitle param
    GL.lineSmooth $= GL.Enabled
    GL.blend      $= GL.Enabled
    GL.blendFunc  $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    GL.shadeModel $= GL.Flat
    GL.textureFunction $= GL.Combine

    let Color r g b a = _clearColor param in GL.clearColor $= GL.Color4 (gf r) (gf g) (gf b) (gf a)

    res <- runFinalizerT m

    GLFW.closeWindow
    GLFW.terminate
    return res
