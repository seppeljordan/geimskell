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

.PHONY: run build shell
