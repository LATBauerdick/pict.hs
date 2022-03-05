{ nixpkgs ? import <nixpkgs> {},
  compiler ? "ghc8107" }:
  (import ./default.nix {
    inherit nixpkgs compiler;
    executableSystemDepends =
      with nixpkgs.haskell.packages.${compiler}; 
      [
        cabal-install
        ghcid
      ];
  }).env

