#! /usr/bin/env nix-shell
#! nix-shell -i bash ../shell.nix

# Compile against sdl2-image for the IMG_SavePNG binding.
# This is fragile as it relies on hard-coded path in the Nix store.
# TODO I guess I have to use Cabal to properly set those ?
ghc -O2 --make bin/loading.hs \
  -lSDL2_image \
  -I/nix/store/yzhqcvnhrbr6h1cdnw7pdb37jxg5cl34-SDL2_image-2.0.5/include/SDL2/ \
  -L/nix/store/yzhqcvnhrbr6h1cdnw7pdb37jxg5cl34-SDL2_image-2.0.5/lib
