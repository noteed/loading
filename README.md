# Loading

Just trying out the SDL2 library with Haskell.

- This uses a `shell.nix` file to bring in the necessary dependencies to build
  the code.
- This uses `direnv` (and thus a `.envrc`) with the `use_nix` feature to
  automatically use the above `shell.nix` upon entering this directory.

The code doesn't do much: it displays a window, and a littel dot. It can be
moved with the arrow keys, a dot can be drawn by pressing `enter`, and `q`
exits the program.


## Building

To build the binary at `bin/loading`, simply run

```
$ make
```

To validate the code during developement, use `ghcid` by running:

```
$ scripts/ghcid.sh
```

As a convenience, it is possible to run the executable, ensuring it is built
only if necessary:

```
$ make run
```


## Current status

- The rendering is done with a low resolution, similar to old games.
- Some basic shapes (point, line, triangle) are rendered.
- Some commands can be done with a gamepad.
- Left mouse button clicks change the cursor location, and sets the magnified
  zone location.
- Pressing `q` quits the application.
- Pressing `e` enables/disables logging polled events.
- Pressing `m` or right shoulder button show/hide a magnified zone of the
  screen.
- Pressing the arrow keys or d-pad move the cursor.
- Pressing `space` or "A" button draw a point where the cursor is.
- `loading headless` generates directly a screenshot, without actually opening
  an interactive window.
- A crude fixed frame rate is implemented. This limits CPU usage to 2% (window
  on a hidden workspace) or 5% (window visible) on my T480, instead of 99%
  (hidden) or 50% (visible).

When quitting, a screenshot is captured as `screenshot.png`.

A nice way to view the screenshot that mimics the behavior of the SDL window
under my xmonad configration is using the following helper that uses `feh`:

```
$ make view
```

This creates a floating, centered window with the 384x240 image scaled,
conserving sharp "big" pixels.


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

I was unsure if my X Box controller would work. I plugged it in (wired), and
checked `dmesg`:

```
[16352.189193] input: Microsoft X-Box One S pad as /devices/pci0000:00/0000:00:14.0/usb1/1-1/1-1:1.0/input/input21
[16352.189271] usbcore: registered new interface driver xpad
```

This looked good and I confirmed it was working with `jstest /dev/input/js0`.

Note: the build scripts hard-code paths into the Nix store to build against the
sdl2 image library. This should be fixed.
