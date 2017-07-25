{ pkgs ? import <nixpkgs> {} }:
let
  overrides = import ./overrides.nix;
  haskellPackages = pkgs.haskellPackages.override
    { overrides = overrides; };
in
haskellPackages.geimskell

