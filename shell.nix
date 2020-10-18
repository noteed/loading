{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    buildInputs = [
      pkgs.ghcid
      (pkgs.haskellPackages.ghcWithPackages (hpkgs: [
        hpkgs.sdl2
        hpkgs.sdl2-gfx
      ]))
    ];
}
