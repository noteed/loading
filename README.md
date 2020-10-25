# Loading

Just trying out the SDL2 library with Haskell.

- This uses a `shell.nix` file to bring in the necessary dependencies to build
  the code.
- This uses `direnv` (and thus a `.envrc`) with the `use_nix` feature to
  automatically use the above `shell.nix` upon entering this directory.

The code doesn't do much: it displays a red window, and waits the for the `q`
key to be pressed, then exits.


## Building

To build the binary at `bin/loading`, simply run

```
$ scripts/build.sh
```

To validate the code during developement, use `ghcid` by running:

```
$ scripts/ghcid.sh
```


## Current status

- The rendering is done with a low resolution, similar to old games.
- Some basic shapes (point, line, triangle) are rendered.
- Left mouse button clicks add points.
- Pressing `q` quits the application.
- Pressing `m` shows/hides a magnified zone of the screen.
- Pressing the arrow keys moves the magnified zone selection.


## Notes

The `defaultWindow` has its `windowResizable` to False. With XMonad, this makes
the window appear at the requested size and centered (above tiled windows).
Setting `windowResizable` to True instead makes it behave like a regular
window: when it appears, it is tiled among the other windows.

To obtain "big" pixels, i.e. an appearance of a low resolution image similar to
old games, there is a `rendererLogicalSize` function. At first it seems to
work, but actually some drawing routines are not properly scaled (e.g.
`drawLine`, or `triangle` from the sdl-gfx library). Instead I have used a low
resolution texture as rendering target, which is then copied to the default
render target.
