{ pkgs ? import <nixpkgs> {} }:
let
  overrides = import ./overrides.nix;
  haskellPackages = pkgs.haskellPackages.override
    { overrides = overrides; };
in
{
  release = haskellPackages.geimskell;
  shell = haskellPackages.geimskell.env;
}
