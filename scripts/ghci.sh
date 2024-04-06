#! /usr/bin/env nix-shell
#! nix-shell -i bash ../shell.nix

ghc --interactive \
  -v0 \
  -ibin/ \
  -fhide-source-paths \
  -XImportQualifiedPost \
  -XLambdaCase \
  -XOverloadedStrings \
  -XRecordWildCards \
  -XStrictData \
  -XTypeApplications \
  -XTypeOperators \
  -Wall \
  -Wno-type-defaults \
  -ghci-script scripts/ghci.conf
