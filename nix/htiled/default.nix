{ mkDerivation, base, base64-bytestring, bytestring, fetchgit
, filepath, hspec, hxt, QuickCheck, split, stdenv, vector, xmlgen
, zlib
}:
mkDerivation {
  pname = "htiled";
  version = "0.1.4.0";
  src = fetchgit {
    url = "https://github.com/seppeljordan/htiled.git";
    sha256 = "168fnnw4pjkdc1rkyazj5d860k8lzr4ici1hjz0bvzgm13s09yqj";
    rev = "aad7ddb02cfbbbbc56b7191076e7cae9d408aba3";
  };
  libraryHaskellDepends = [
    base base64-bytestring bytestring filepath hxt split vector zlib
  ];
  testHaskellDepends = [
    base bytestring hspec hxt QuickCheck xmlgen
  ];
  description = "Import from the Tiled map editor";
  license = stdenv.lib.licenses.bsd3;
}
