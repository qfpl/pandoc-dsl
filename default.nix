{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  sources = {
    pandoc = pkgs.fetchFromGitHub {
      owner = "jgm";
      repo = "pandoc";
      rev = "72878adc63f6a1e5178734aab499c3cd10df6016";
      sha256 = "1bl0nq555fr5n5w4rwhb3y4ggk23kljq7v1bz14zgs5sx90ri0in";
    };
  };

  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      pandoc = self.callCabal2nix "pandoc" sources.pandoc { };
      # hslua-module-text = pkgs.haskell.lib.doJailbreak super.hslua-module-text;
    };
  };

  pandoc-dsl = modifiedHaskellPackages.callPackage ./pandoc-dsl.nix {};

in
  pandoc-dsl
