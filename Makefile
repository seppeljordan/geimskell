GHC_OPTIONS=--ghc-options=\"-Wall -threaded\"

run:
	nix-shell --command "cabal run $(GHC_OPTIONS)"

build:
	nix-shell --command "cabal build $(GHC_OPTIONS)"

shell:
	nix-shell

configure:
	nix-shell --command "cabal configure"

haddock:
	nix-shell --command "cabal haddock --executable"

test:
	nix-shell --command "cabal test"

.PHONY: run build shell configure haddock test sound-server
