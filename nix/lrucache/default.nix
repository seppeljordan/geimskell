{ mkDerivation, base, Cabal, containers, contravariant, fetchgit
, hspec, hspec-core, QuickCheck, stdenv
}:
mkDerivation {
  pname = "lrucache";
  version = "1.2.0.0";
  src = fetchgit {
    url = "https://github.com/seppeljordan/lrucache.git";
    sha256 = "17yg3x7cwkgb70mg9ffqzlm5g6p9jsq1ypr31bcnkan09nncx4lf";
    rev = "ad43bf21ec10e77abf43e59890d4cbaf3a684592";
  };
  libraryHaskellDepends = [ base containers contravariant ];
  testHaskellDepends = [ base Cabal hspec hspec-core QuickCheck ];
  homepage = "http://github.com/chowells79/lrucache";
  description = "a simple, pure LRU cache";
  license = stdenv.lib.licenses.bsd3;
}
