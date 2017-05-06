{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, csound-expression, deepseq, lens
      , linear, mtl, random, reactive-banana, sdl2, sdl2-compositor
      , stdenv, text, transformers, csound, QuickCheck, process, stm
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
        executableSystemDepends = [ csound_custom ];
        executableHaskellDepends = [
          base csound-expression deepseq lens linear mtl random
          reactive-banana sdl2 sdl2-compositor text transformers
          QuickCheck process stm
        ];
        license = stdenv.lib.licenses.gpl3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
