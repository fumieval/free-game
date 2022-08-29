1.2
---------------------------------------------------------------------

* Removed `Drawable`, `reGame` and `reFrame`
* Removed `FromFinalizer` and `embedIO` in favour of `MonadResource` and `liftIO`
* Thoroughly cleaned up the codebase

1.1.80
---------------------------------------------------------------------
* Added `mouseScroll`
* Fixed the malfunction of FPS management

1.1.79
---------------------------------------------------------------------
* Exported `clipBitmap`

1.1.78
* Added `mouseInWindow`

1.1
-------------------------------------------------------------------------
* Use Box instead of drab BoundingBox

1.0.5
-------------------------------------------------------------------------
* No fundamental changes

1.0.4
-------------------------------------------------------------------------

* Fixed some potential bugs that appeared on 7.8.1 RC2
* Added `getBoundingBox` and `setBoundingBox` which accesses the window size and the region to draw.
* `Resizable`, the new constructor of `WindowMode`, will create a resizable window.
* Demoted the precedence of `thickness` and `blendMode` according to other APIs.

1.0.3
-------------------------------------------------------------------------
* Added `runGameDefault` as an alternative of classic `runGame def`.
* Removed the duplicate instance of `MonadIO`.
* `free-game` no longer depends on ominous `repa`.
* Reconstructed 'FreeGame.Data.Bitmap'. 'Bitmap' is just an alias of `Codec.Picture.Repa.Image PixelRGBA8`
* Added `bitmapOnce` which does not keep the internal texture to draw.
* Added `forkFrame` analogous to `forkIO`.
* Accelerate text rendering.
* Make the window size solid.

1.0.2
-------------------------------------------------------------------------
* Supported changing a blend function. `blendMode mode m` changes the blend mode while `m` is running.
* Fixed fatal 'keyPress'-related bugs.
* Special thanks: [@myuon_myon](https://twitter.com/myuon_myon)
* Re-added `keyChar` and `keySpecial`.

1.0.1
-------------------------------------------------------------------------
* Demoted the precedence of `Affine` APIs to 5.

1.0
-------------------------------------------------------------------------
* Supported free-4.4.
* Supported GLFW-b-1.3.
* Use `Double` instead of `Float`.
* Made it more efficient.
* `loadBitmaps` takes an expression instead of a `Name`.
* Reorganized typeclasses.
* Rename: `fromBitmap` -> `bitmap`
* Rename: `colored` -> `color`
* New API: `takeScreenshot`
* New API: `getFPS`, `setFPS`
* Now the verbose module prefix `Graphics.UI` is extinct.
* And a bunch of renovations...