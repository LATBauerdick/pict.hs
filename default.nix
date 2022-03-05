# default.nix
#let 
#  pkgs = import <nixpkgs> { };
#in
#  pkgs.haskellPackages.developPackage {
#    root = ./.;
#    modifier = drv:
#      pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
#        [ cabal-install
#          ghcid
#        ]);
#  }

{ nixpkgs ? import <nixpkgs> {}
  , compiler ? "ghc8107"
  , executableSystemDepends ? []
}: nixpkgs.pkgs.haskell.packages.${compiler}.callPackage
    ./pict.nix {  inherit executableSystemDepends; }
