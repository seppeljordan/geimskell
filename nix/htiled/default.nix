{ mkDerivation, base, base64-bytestring, bytestring, fetchgit
, filepath, hxt, split, stdenv, vector, zlib
}:
mkDerivation {
  pname = "htiled";
  version = "0.1.4.0";
  src = fetchgit {
    url = "https://github.com/seppeljordan/htiled.git";
    sha256 = "1bdsrqcxjbh0qrbf3qbc5ckha0j636m1z0q0yfjgvaklji7j5d21";
    rev = "536651727199e4e90e84d9e52df1b10454d30966";
  };
  libraryHaskellDepends = [
    base base64-bytestring bytestring filepath hxt split vector zlib
  ];
  description = "Import from the Tiled map editor";
  license = stdenv.lib.licenses.bsd3;
}
