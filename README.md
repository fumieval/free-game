free-game
=========

[![Build Status](https://secure.travis-ci.org/fumieval/free-game.png?branch=master)](http://travis-ci.org/fumieval/free-game)

free-game is a library that abstracts graphical applications with simple interfaces.

Install
-------------------------------------------------------------------------------------

    $ cabal install free-game


Migration Guide
-------------------------------------------------------------------------------------

* `runGame param`
    * `runGame Window (BoundingBox 0 0 640 480)` or `runGame FullScreen (BoundingBox 0 0 640 480)`
    * `setTitle "Lorem ipsum"`
    * `showCursor` or `hideCursor`
    * `clearColor black`
    * `setFPS 60`

* `loadBitmapsWith 'func "/path/to"`
    * `loadBitmapsWith [|func|] "/path/to"`
* `keySpecial`
    * `keyPress` or `keyDown` or `keyUp` -- You don't have to keep key states anymore!
* `foreverTick m`
    * `foreverFrame m` -- It is faster
* do { some draw-only computation }
    * Just apply `draw` to make your code faster.

Special Thanks
------------------------------------------------------------------------------------

* [jbracker](https://github.com/jbracker), extended features and fixed minor bugs

Bug reports, pull requests, feature requests are welcome.

Donate
-------------------------------------

Bitcoin Address: 1EvewG7YHdcgMjmQEYEBrKiPjUwGWBckY4
