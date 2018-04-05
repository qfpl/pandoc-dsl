{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  pandoc-dsl = haskellPackages.callPackage ./pandoc-dsl.nix {};

in
  pandoc-dsl
