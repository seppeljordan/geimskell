{ mkDerivation, base, Cabal, fetchgit, hspec, hspec-core, lens
, linear, lrucache, QuickCheck, sdl2, StateVar, stdenv, stm, text
, transformers
}:
mkDerivation {
  pname = "sdl2-compositor";
  version = "1.2.0.7";
  src = fetchgit {
    url = "https://github.com/seppeljordan/sdl2-compositor.git";
    sha256 = "04fqhzf0z0a0j7cc8yhqklpx3xijq1hsrpwah1520q83lbas16g4";
    rev = "a0e6f0bf9b3b8a19202ec798feb21ddb64affd66";
    fetchSubmodules = true;
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
