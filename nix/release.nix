{ pkgs ? import <nixpkgs> {} }:
let
  overrides = import ./overrides.nix;
  haskellPackages = pkgs.haskellPackages.override
    { overrides = overrides; };
in
rec {
  release = haskellPackages.geimskell;
  shell = release.env.overrideAttrs( old: {
    shellHook = old.shellHook + ''
      # We need cabal-install to be available
      PATH=${pkgs.cabal-install}/bin:${pkgs.csound}/bin:$PATH
    '';
  });
}
