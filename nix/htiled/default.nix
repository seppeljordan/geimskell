{ mkDerivation, base, base64-bytestring, bytestring, fetchgit
, filepath, hspec, hxt, QuickCheck, split, stdenv, text, vector
, xmlgen, zlib
}:
mkDerivation {
  pname = "htiled";
  version = "0.2.0.0";
  src = fetchgit {
    url = "https://github.com/seppeljordan/htiled.git";
    sha256 = "1gzk9fp2yv4lcf97bglci8478yqjnlzp2h4rwxr1lj915993f5fr";
    rev = "102dbd871a0a1203628be2ba502afc55e5c30798";
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
