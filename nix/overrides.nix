self: super:
{
  htiled = self.callPackage htiled/default.nix {};
  sdl2-compositor = self.callPackage sdl2-compositor/default.nix {};
  geimskell = self.callPackage ./geimskell.nix {};
  mkDerivation = args: super.mkDerivation ( args // {
    enableLibraryProfiling = true;
  });
}
