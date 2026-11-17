.PHONY: build test repl clean shots bundle

VERSION := $(shell sed -n 's/^version: *//p' datomlite.cabal)
BUNDLE_DIR := bundles

build:
	cabal build

test:
	cabal test --test-show-details=streaming

repl:
	cabal repl

clean:
	cabal clean
	rm -rf $(BUNDLE_DIR)

bundle: build test
	cabal check
	@rm -rf $(BUNDLE_DIR) && mkdir -p $(BUNDLE_DIR)
	cabal sdist -o $(BUNDLE_DIR)
	cabal haddock --haddock-for-hackage --builddir=dist-newstyle
	cp dist-newstyle/datomlite-$(VERSION)-docs.tar.gz $(BUNDLE_DIR)/
	@echo "\nBundle ready: $(BUNDLE_DIR)/datomlite-$(VERSION).tar.gz"

# Render the README REPL screenshot from demos/Repl.hs (a real cabal repl run)
shots:
	python3 demos/render.py demos/Repl.hs ../pix/datomlite-hs-repl.png
