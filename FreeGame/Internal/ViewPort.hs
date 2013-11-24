module Graphics.UI.FreeGame.Internal.ViewPort where
import Unsafe.Coerce

data ViewPort a = ViewPort (Vec2 -> Vec2) (Vec2 -> Vec2)

instance Affine ViewPort where
    translate v (ViewPort f g) = ViewPort ((^+^v) . f) (g . (^-^v))
    rotateR t (ViewPort f g) = ViewPort (rot2 t . f) (g . rot2 (-t))
    scale v (ViewPort f g) = ViewPort ((*v) . f) (g . (/v))

coerceViewPort :: ViewPort a -> ViewPort b
coerceViewPort = unsafeCoerce

flipViewPort :: ViewPort a -> ViewPort b
flipViewPort (ViewPort f g) = ViewPort g f