{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, csound-expression, deepseq, lens
      , linear, mtl, random, reactive-banana, sdl2, sdl2-compositor
      , stdenv, text, transformers, csound, QuickCheck, process, stm
      , hosc, sdl2-image, tiled, htiled, array, containers
      , prelude-safeenum, hspec
      }:
      let
        csound_custom = csound.overrideDerivation(old: {
          buildInputs = old.buildInputs ++ [ pkgs.liblo ];
        });
      in
      mkDerivation {
        pname = "spaceshooter";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableSystemDepends = [ csound_custom tiled ];
        executableHaskellDepends = [
          base csound-expression deepseq lens linear mtl random
          reactive-banana sdl2 sdl2-compositor text transformers
          process stm hosc sdl2-image htiled array
          containers prelude-safeenum
        ];
        testHaskellDepends = [ base hspec QuickCheck ];
        license = stdenv.lib.licenses.gpl3;
      };

  haskellPackagesOrig = if compiler == "default"
                        then pkgs.haskellPackages
                         else pkgs.haskell.packages.${compiler};
  haskellPackages = haskellPackagesOrig.override {
    overrides = import nix/overrides.nix;
  };
  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
