{ mkDerivation, base, checkers, containers, hedgehog, lens
, pandoc-lens, pandoc-types, papa, QuickCheck, stdenv, tasty
, tasty-hedgehog, tasty-hunit, tasty-quickcheck
}:
mkDerivation {
  pname = "pandoc-dsl";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base lens pandoc-lens pandoc-types papa
  ];
  testHaskellDepends = [
    base checkers containers hedgehog lens pandoc-lens pandoc-types
    papa QuickCheck tasty tasty-hedgehog tasty-hunit tasty-quickcheck
  ];
  homepage = "https://github.com/qfpl/pandoc-dsl";
  description = "For building pandoc documents";
  license = stdenv.lib.licenses.bsd3;
}
