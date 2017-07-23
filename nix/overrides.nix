self: super:

{
  htiled = self.callPackage htiled/default.nix {};
  sdl2-compositor = self.callPackage sdl2-compositor/default.nix {};
  lrucache = self.callPackage lrucache/default.nix {};
}
