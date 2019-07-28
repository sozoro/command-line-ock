{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc865" }:
let
  config = {
    packageOverrides = super: let self = super.pkgs; in {
      haskellPackages = super.haskellPackages.override {
        overrides = self: super: {
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };
in
  nixpkgs.pkgs.haskell.packages.${compiler}.callPackage
    ./command-line-ock.nix {}
