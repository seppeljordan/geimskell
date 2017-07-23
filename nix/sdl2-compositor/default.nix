{ mkDerivation, base, Cabal, fetchgit, hspec, hspec-core, lens
, linear, lrucache, QuickCheck, sdl2, StateVar, stdenv, stm, text
, transformers
}:
mkDerivation {
  pname = "sdl2-compositor";
  version = "1.2.0.7";
  src = fetchgit {
    url = "https://github.com/seppeljordan/sdl2-compositor.git";
    sha256 = "053p4ni4wr2iw1a82w3413vl03833pdfx9bqgag1h83p73vwg8iz";
    rev = "510fd8400cb148001e07f759fae18fa33330ea0b";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base lens linear lrucache QuickCheck sdl2 StateVar stm text
    transformers
  ];
  testHaskellDepends = [
    base Cabal hspec hspec-core lrucache QuickCheck stm
  ];
  description = "image compositing with sdl2 - declarative style";
  license = stdenv.lib.licenses.gpl3;
}
