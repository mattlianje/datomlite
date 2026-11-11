.PHONY: build test repl clean shots

build:
	cabal build

test:
	cabal test --test-show-details=streaming

repl:
	cabal repl

clean:
	cabal clean

# Render the README REPL screenshot from demos/Repl.hs (a real cabal repl run)
shots:
	python3 demos/render.py demos/Repl.hs ../pix/datomlite-hs-repl.png
