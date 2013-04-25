-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.FreeGame.Data.Font
-- Copyright   :  (C) 2013 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinsohita <fumiexcel@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
-- Rendering characters
----------------------------------------------------------------------------
module Graphics.UI.FreeGame.Data.Font 
  ( Font
  , loadFont
  , Metrics(..)
  , Graphics.UI.FreeGame.Data.Font.metrics
  , fontBoundingBox
  , charToBitmap
  , RenderedChar(..)
  , text
  , renderCharacters
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Data.Array.Repa as R
import Data.Array.Repa.Eval
import qualified Data.Map as M
import Data.Word
import Linear
import Graphics.UI.FreeGame.Base
import Graphics.UI.FreeGame.Types
import Graphics.UI.FreeGame.Data.Bitmap
import Graphics.UI.FreeGame.Internal.Finalizer
import Graphics.Rendering.FreeType.Internal
import qualified Graphics.Rendering.FreeType.Internal.GlyphSlot as GS
import qualified Graphics.Rendering.FreeType.Internal.Vector as V
import Graphics.Rendering.FreeType.Internal.Bitmap as B
import Graphics.Rendering.FreeType.Internal.PrimitiveTypes as PT
import Graphics.Rendering.FreeType.Internal.Face as F
import Graphics.Rendering.FreeType.Internal.Library as L
import Graphics.Rendering.FreeType.Internal.BBox as BB
import Foreign.Marshal.Alloc
import Foreign.C.Types
import Foreign.C.String
import Foreign.Storable
import System.IO.Unsafe
import Unsafe.Coerce

-- | Font object
data Font = Font FT_Face Metrics (BoundingBox Float) (IORef (M.Map (Float, Char) RenderedChar))

-- | Create a 'Font' from the given file.
loadFont :: FilePath -> IO Font
loadFont path = alloca $ \p -> do
    e <- withCString path $ \str -> ft_New_Face freeType str 0 p
    failFreeType e
    f <- peek p
    b <- peek (bbox f)
    asc <- peek (ascender f)
    desc <- peek (descender f)
    u <- fromIntegral <$> peek (units_per_EM f)
    let m = Metrics (fromIntegral asc/u) (fromIntegral desc/u)
        box = BoundingBox (V2 (fromIntegral (xMin b)/u) (fromIntegral (yMin b)/u))
                          (V2 (fromIntegral (xMax b)/u) (fromIntegral (yMin b)/u))
    Font f m box <$> newIORef M.empty

-- | Get the font's metrics.
metrics :: Font -> Metrics
metrics (Font _ m _ _) = m

fontBoundingBox :: Font -> BoundingBox Float
fontBoundingBox (Font _ _ b _) = b

-- | Render a text by the specified 'Font'.
text :: (Monad p, Picture2D p, FromFinalizer p) => Font -> Float -> String -> p ()
text font siz str = join $ fromFinalizer $ fmap sequence_ (renderCharacters font siz str)

failFreeType :: Monad m => CInt -> m ()
failFreeType 0 = return ()
failFreeType e = fail $ "FreeType Error:" Prelude.++ show e

freeType :: FT_Library
freeType = unsafePerformIO $ alloca $ \p -> do
    failFreeType =<< ft_Init_FreeType p
    peek p

data RenderedChar = RenderedChar
    { charBitmap :: Bitmap
    , charOffset :: V2 Float
    ,　charAdvance :: Float
    }

data Metrics = Metrics
    { metricsAscent :: Float
    , metricsDescent :: Float
    }

-- | The resolution used to render fonts.
resolutionDPI :: Int
resolutionDPI = 300

charToBitmap :: Font -> Float -> Char -> FinalizerT IO RenderedChar
charToBitmap (Font face _ _ refCache) pixel ch = do
    cache <- liftIO $ readIORef refCache
    case M.lookup (siz, ch) cache of
        Nothing -> do
            d <- liftIO render
            liftIO $ writeIORef refCache $ M.insert (siz, ch) d cache
            finalizer $ modifyIORef refCache $ M.delete (siz, ch)
            return d
        Just d -> return d
    where
        siz = pixel * 72 / fromIntegral resolutionDPI
        render = do
            let dpi = fromIntegral resolutionDPI

            failFreeType =<< ft_Set_Char_Size face 0 (floor $ siz * 64) dpi dpi
            
            ix <- ft_Get_Char_Index face (fromIntegral $ fromEnum ch)
            failFreeType =<< ft_Load_Glyph face ix ft_LOAD_DEFAULT

            slot <- peek $ glyph face
            failFreeType =<< ft_Render_Glyph slot ft_RENDER_MODE_NORMAL

            bmp <- peek $ GS.bitmap slot
            left <- fmap fromIntegral $ peek $ GS.bitmap_left slot
            top <- fmap fromIntegral $ peek $ GS.bitmap_top slot

            let h = fromIntegral $ B.rows bmp
                w = fromIntegral $ B.width bmp
                
            mv <- newMVec (w * h)

            fillChunkedIOP (w * h) (unsafeWriteMVec mv) $ const $ return
                $ fmap unsafeCoerce . peekElemOff (buffer bmp)

            adv <- peek $ GS.advance slot

            ar <- unsafeFreezeMVec (Z:.h:.w) mv :: IO (R.Array U DIM2 Word8)

            let pix (crd:.0) = R.index ar crd
                pix (_:._) = 255

            result <- computeP (fromFunction (Z:.h:.w:.4) pix) >>= makeStableBitmap
            
            return $ RenderedChar result (V2 left (-top)) (fromIntegral (V.x adv) / 64)
 
renderCharacters :: (Monad p, Picture2D p) => Font -> Float -> String -> FinalizerT IO [p ()]
renderCharacters font pixel str = render str 0 where
    render [] _ = return []
    render (c:cs) pen = do
        RenderedChar b (V2 x y) adv <- charToBitmap font pixel c
        let (w,h) = bitmapSize b
            offset = V2 (pen + x + fromIntegral w / 2) (y + fromIntegral h / 2)
        (translate offset (fromBitmap b):) <$> render cs (pen + adv)
