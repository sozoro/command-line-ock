{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc863"}:
(nixpkgs.pkgs.haskell.packages.${compiler}.developPackage { root = ./.; })
  .overrideAttrs (oldAttrs: with nixpkgs; {
    buildInputs = oldAttrs.buildInputs ++ [ git cabal-install zsh vim less ]; # haskell.packages.ghc862.threadscope ];
    shellHook = ''
      zsh -c "PATH=$PATH; TZ="Asia/Tokyo"; zsh"
      exit
    '';
  })
