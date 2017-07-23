{ mkDerivation, base, Cabal, containers, contravariant, fetchgit
, hspec, hspec-core, QuickCheck, stdenv
}:
mkDerivation {
  pname = "lrucache";
  version = "1.2.0.0";
  src = fetchgit {
    url = "https://github.com/seppeljordan/lrucache.git";
    sha256 = "1ypcajyq9sas57l0gpcdd26j07js8nmjh7x6x70rm6n0b4bhvi73";
    rev = "84fad72009078a67bd6942a6a7e0ffa80959659e";
  };
  libraryHaskellDepends = [ base containers contravariant ];
  testHaskellDepends = [ base Cabal hspec hspec-core QuickCheck ];
  homepage = "http://github.com/chowells79/lrucache";
  description = "a simple, pure LRU cache";
  license = stdenv.lib.licenses.bsd3;
}
