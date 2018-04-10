{ mkDerivation, base, checkers, containers, hedgehog, lens, pandoc
, pandoc-lens, pandoc-types, papa, QuickCheck, stdenv, tasty
, tasty-hedgehog, tasty-hunit, tasty-quickcheck, text
}:
mkDerivation {
  pname = "pandoc-dsl";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base lens pandoc pandoc-lens pandoc-types papa text
  ];
  testHaskellDepends = [
    base checkers containers hedgehog lens pandoc pandoc-lens
    pandoc-types papa QuickCheck tasty tasty-hedgehog tasty-hunit
    tasty-quickcheck text
  ];
  homepage = "https://github.com/qfpl/pandoc-dsl";
  description = "For building pandoc documents";
  license = stdenv.lib.licenses.bsd3;
}
