module Graphics.UI.FreeGame.Internal.GLFW where

data System = System
    { refFrameCounter :: IORef Int
    , refFPS :: IORef Int
    , refTextures :: IORef Texture
    }

fromKeyState :: KeyState -> Bool
fromKeyState KeyState'Pressed = True
fromKeyState KeyState'Released = False
fromKeyState KeyState'Repeating = True

runVertices :: MonadIO m => [V2 Double] -> m ()
runVertices = liftIO . mapM_ (GL.vertex . mkVertex2)
{-# INLINE runVertices #-}

preservingMatrix' :: MonadIO m => m a -> m a
preservingMatrix' m = do
    liftIO glPushMatrix
    r <- m
    liftIO glPopMatrix
    return r

drawTexture :: Texture -> IO ()
drawTexture (tex, w, h) = drawTextureAt tex (V2 (-w) (-h)) (V2 w (-h)) (V2 w h) (V2 (-w) h)
{-# INLINE drawTexture #-}

drawTextureAt :: GL.TextureObject -> V2 Double -> V2 Double -> V2 Double -> V2 Double -> IO ()
drawTextureAt tex a b c d = do
    GL.texture GL.Texture2D $= GL.Enabled
    GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)
    GL.textureBinding GL.Texture2D $= Just tex
    GL.unsafeRenderPrimitive GL.TriangleStrip $ do
        GL.texCoord $ GL.TexCoord2 (0 :: GL.GLdouble) 0
        GL.vertex $ mkVertex2 a
        GL.texCoord $ GL.TexCoord2 (1 :: GL.GLdouble) 0
        GL.vertex $ mkVertex2 b
        GL.texCoord $ GL.TexCoord2 (0 :: GL.GLdouble) 1
        GL.vertex $ mkVertex2 d
        GL.texCoord $ GL.TexCoord2 (1 :: GL.GLdouble) 1
        GL.vertex $ mkVertex2 c
    GL.texture GL.Texture2D $= GL.Disabled

mkVertex2 :: V2 Double -> GL.Vertex2 GL.GLdouble
{-# INLINE mkVertex2 #-}
mkVertex2 = unsafeCoerce

gf :: Float -> GL.GLfloat
{-# INLINE gf #-}
gf = unsafeCoerce

gd :: Double -> GL.GLdouble
{-# INLINE gd #-}
gd = unsafeCoerce

gsizei :: Int -> GL.GLsizei
{-# INLINE gsizei #-}
gsizei = unsafeCoerce

translate (V2 tx ty) m = preservingMatrix' $ liftIO (GL.translate (GL.Vector3 (gf tx) (gf ty) 0)) >> m

rotate theta m = preservingMatrix' $ liftIO (GL.rotate (gf (-theta)) (GL.Vector3 0 0 1)) >> m

scale (V2 sx sy) m = preservingMatrix' $ liftIO (GL.scale (gf sx) (gf sy) 1) >> m

circle r = do
    let s = 2 * pi / 64
    GL.renderPrimitive GL.Polygon $ runVertices [V2 (cos t * r) (sin t * r) | t <- [0,s..2 * pi]]
circleOutline r = do
    let s = 2 * pi / 64 * sc
    GL.renderPrimitive GL.LineLoop $ runVertices [V2 (cos t * r) (sin t * r) | t <- [0,s..2 * pi]]

colored col m = do
    oldColor <- liftIO $ get GL.currentColor
    liftIO $ GL.currentColor $= unsafeCoerce col
    res <- m
    liftIO $ GL.currentColor $= oldColor
    return res

polygon path = GL.renderPrimitive GL.Polygon $ runVertices path
polygonOutline path = GL.renderPrimitive GL.LineLoop $ runVertices path
line path = GL.renderPrimitive GL.LineStrip $ runVertices path

thickness t m = do
    oldWidth <- liftIO $ get GL.lineWidth
    liftIO $ GL.lineWidth $= gf t
    res <- runUI f inner
    liftIO $ GL.lineWidth $= oldWidth
    return res

installTexture :: Bitmap -> FinalizerT IO Texture
installTexture bmp@(BitmapData ar _) = do
    [tex] <- liftIO $ GL.genObjectNames 1
    liftIO $ GL.textureBinding GL.Texture2D GL.$= Just tex
    let (width, height) = bitmapSize bmp
    let siz = GL.TextureSize2D (gsizei width) (gsizei height)
    liftIO $ withForeignPtr (RF.toForeignPtr ar)
        $ GL.texImage2D Nothing GL.NoProxy 0 GL.RGBA8 siz 0
        . GL.PixelData GL.RGBA GL.UnsignedInt8888
    finalizer $ GL.deleteObjectNames [tex]
    return (tex, fromIntegral width / 2, fromIntegral height / 2)

beginFrame :: IO ()
beginFrame = do
    GL.clear [GL.ColorBuffer] 
    GL.loadIdentity
    GL.scale (gf 1) (-1) 1
    let V2 ox oy = _windowOrigin param
        V2 ww wh = _windowSize param
        windowL = realToFrac ox
        windowR = realToFrac ox + fromIntegral ww
        windowT = realToFrac oy
        windowB = realToFrac oy + fromIntegral wh
    GL.ortho windowL windowR windowT windowB 0 (-100)
    GL.matrixMode $= GL.Modelview 0

endFrame :: IO ()
    GL.matrixMode $= GL.Projection
    GLFW.swapBuffers
    performGC
    t <- GLFW.getTime
    n <- readIORef given
    GLFW.sleep $ fromIntegral n / fromIntegral (_framePerSecond param) - t
    if t > 1
        then GLFW.setTime 0 >> writeIORef given 0
        else writeIORef given (succ n)

