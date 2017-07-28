{ mkDerivation, base, base64-bytestring, bytestring, fetchgit
, filepath, hspec, hxt, QuickCheck, split, stdenv, text, vector
, xmlgen, zlib
}:
mkDerivation {
  pname = "htiled";
  version = "0.1.4.0";
  src = fetchgit {
    url = "https://github.com/seppeljordan/htiled.git";
    sha256 = "1810g5alsqzqs8w4y75c5cm272w2f2pc9kdz3c6vp3hy4cm54s56";
    rev = "1ab52aaa8565f99a782587e11b8a581448a925f1";
  };
  libraryHaskellDepends = [
    base base64-bytestring bytestring filepath hxt split vector zlib
  ];
  testHaskellDepends = [
    base bytestring hspec hxt QuickCheck text xmlgen
  ];
  description = "Import from the Tiled map editor";
  license = stdenv.lib.licenses.bsd3;
}
