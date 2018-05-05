GHC_OPTIONS=--ghc-options=\"-Wall -threaded -fno-warn-missing-signatures\"
NIX_PATH=nixpkgs=https://github.com/NixOS/nixpkgs-channels/archive/nixpkgs-unstable.tar.gz
NIX_SHELL=nix-shell nix/release.nix -A shell
NIX_SHELL_PURE=nix-shell nix/release.nix -A shell --pure

run:
	$(NIX_SHELL) --command "exec cabal run -- --sound-engine csound"

run-nosound:
	$(NIX_SHELL) --command "exec cabal run -- --sound-engine none"

build:
	$(NIX_SHELL_PURE) --command "exec cabal build"

release:
	rm -rf dist/
	nix-build nix/release.nix -A release -o result

shell:
	$(NIX_SHELL)

configure:
	$(NIX_SHELL_PURE) --command "exec cabal configure $(GHC_OPTIONS)"

haddock:
	$(NIX_SHELL_PURE) --command "exec cabal haddock --executable"

test:
	$(NIX_SHELL_PURE) --command "exec cabal test"

update-htiled:
	cabal2nix https://github.com/seppeljordan/htiled.git > \
		nix/htiled/default.nix

update-sdl2-compositor:
	cabal2nix https://github.com/seppeljordan/sdl2-compositor.git > \
		nix/sdl2-compositor/default.nix

update-lrucache:
	cabal2nix https://github.com/seppeljordan/lrucache.git > \
		nix/lrucache/default.nix

update-deps: update-htiled update-sdl2-compositor update-lrucache

update-geimskell:
	cd nix && cabal2nix .. > geimskell.nix

.PHONY: run build shell configure haddock test
