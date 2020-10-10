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
