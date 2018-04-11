{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = import ./pandoc-dsl.nix;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};
 
  overriddenHaskellPackages =
    pkgs.haskellPackages.override {
        overrides = self: super: {
          ansi-terminal = self.ansi-terminal_0_7_1_1;
          texmath = self.texmath_0_10_1_1;
          skylighting = self.skylighting_0_6;
          pandoc-types = self.pandoc-types_1_17_3_1;
          doctemplates = self.doctemplates_0_2_1;
          hslua = self.hslua_0_9_5_1;
          pandoc = self.pandoc_2_1_1;
        };
      };

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;


  drv = variant (overriddenHaskellPackages.callPackage f {});

in

  drv
