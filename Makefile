run:
	nix-shell --command "cabal run"

build:
	nix-shell --command "cabal build --ghc-options=\"-Wall\""

shell:
	nix-shell

configure:
	nix-shell --command "cabal configure"

haddock:
	nix-shell --command "cabal haddock --executable"

test:
	nix-shell --command "cabal test"

.PHONY: run build shell
