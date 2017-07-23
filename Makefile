GHC_OPTIONS=--ghc-options=\"-Wall -threaded -fno-warn-missing-signatures\"

run:
	nix-shell --command "exec cabal run $(GHC_OPTIONS)"

build:
	nix-shell --command "exec cabal build $(GHC_OPTIONS)"

shell:
	nix-shell

configure:
	nix-shell --command "exec cabal configure"

haddock:
	nix-shell --command "exec cabal haddock --executable"

test:
	nix-shell --command "exec cabal test"

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

.PHONY: run build shell configure haddock test sound-server
