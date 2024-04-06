# Central overlay that supplies all overlays that:
# 1. Make this package available.
# 2. Provide this particular package with a fixed point of overlayed packages,
#    if they become needed.

{
  compiler ? "ghc928"
}:

let

  sources = import ./sources.nix;
  # inherit (sources) xxx;

  getOverlays = pkg : import "${pkg}/nix/overlays.nix";
  haskellOverlays =
    []
    # ++ getOverlays xxx
    ;

in haskellOverlays ++ [ (import ./overlay.nix { inherit compiler; }) ]
