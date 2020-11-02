#! /usr/bin/env bash

# Compile against sdl2-image for the IMG_SavePNG binding.
# This is fragile as it relies on hard-coded path in the Nix store.
# TODO I guess I have to use Cabal to properly set those ?
ghc --make bin/loading.hs \
  -lSDL2_image \
  -I/nix/store/3n6dpjyw6i3g4pg345zxv0x5wczwg1di-SDL2_image-2.0.5/include/SDL2/ \
  -L/nix/store/3n6dpjyw6i3g4pg345zxv0x5wczwg1di-SDL2_image-2.0.5/lib
