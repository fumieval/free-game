free-game
=========

[![Build Status](https://secure.travis-ci.org/fumieval/free-game.png?branch=master)](http://travis-ci.org/fumieval/free-game)

free-game gives you a world so that you can create games easily.

Install
-------------------------------------------------------------------------------------

    $ cabal update
    $ cabal install free-game

Migration Guide
-------------------------------------------------------------------------------------

* `runGame param`
    * `runGame Window (BoundingBox 0 0 640 480)` or `runGame FullScreen (BoundingBox 0 0 640 480)`
    * `setTitle "Lorem ipsum"`
    * Unicode characters are OK: `setTitle "ニンジャ"`
    * `showCursor` or `hideCursor`
    * `clearColor black`
    * `setFPS 60`

* `loadBitmapsWith 'func "/path/to"`
    * `loadBitmapsWith [|func|] "/path/to"`
* `keySpecial`
    * `keyPress` or `keyDown` or `keyUp` -- You don't have to keep key states anymore!
* `foreverTick m`
    * `foreverFrame m` -- It is faster
* `do { some draw-only computation }`
    * Just apply `draw` to make your code faster.
* `Game` is a kind of coroutine upon `Frame`.

Comparison with [gloss](http://hackage.haskell.org/package/gloss)
-------------------------------------------------------------------------------------
free-game's API is similar to what gloss have.

| gloss         | free-game           |
| ------------- | ------------------- |
| Picture       | Game ()             |
| Blank         | return ()           |
| Text          | text font size      |
| Bitmap w h d  | bitmap              |
| Translate x y | translate (V2 x y)  |
| Scale x y     | scale (V2 x y)      |
| Rotate        | rotate              |
| Color         | color               |
| Pictures xs   | sequence_ xs        |
| Circle        | circle              |
| Line          | line                |
| Polygon       | polygon             |

Special Thanks
------------------------------------------------------------------------------------

* [jbracker](https://github.com/jbracker), extended features and fixed minor bugs

Bug reports, pull requests, feature requests are welcome.

Donate
-------------------------------------

Bitcoin Address: 1EvewG7YHdcgMjmQEYEBrKiPjUwGWBckY4
