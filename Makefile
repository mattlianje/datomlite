.PHONY: compile test test-jvm test-js test-native publish-local bundle clean fmt fmt-check repl svg

VERSION := 0.1.0
SCALA_VERSION := 3.7.3
BUNDLE_DIR := bundles
GPG_KEY := F36FE8EEBD829E6CF1A5ADB6246482D1268EDC6E

compile:
	./mill datomlite.__.compile

repl:
	./mill -i datomlite.jvm[$(SCALA_VERSION)].console

test: test-jvm test-js test-native

test-jvm:
	./mill datomlite.jvm.__.test

test-js:
	./mill datomlite.js.__.test

test-native:
	./mill datomlite.native.__.test

publish-local:
	./mill datomlite.__.publishLocal

fmt:
	./mill mill.scalalib.scalafmt/

fmt-check:
	./mill mill.scalalib.scalafmt/ --check

clean:
	./mill clean
	rm -rf $(BUNDLE_DIR)

bundle:
	@echo "Building publish artifacts..."
	@./mill show datomlite.__.publishArtifacts > /dev/null
	@rm -rf $(BUNDLE_DIR) && mkdir -p $(BUNDLE_DIR)/xyz/matthieucourt
	@for platform in jvm js native; do \
		artifactId=$$(./mill show datomlite.$$platform[$(SCALA_VERSION)].artifactId 2>/dev/null | tr -d '"'); \
		dir=$(BUNDLE_DIR)/xyz/matthieucourt/$$artifactId/$(VERSION); \
		mkdir -p $$dir; \
		cp out/datomlite/$$platform/$(SCALA_VERSION)/pom.dest/*.pom $$dir/$$artifactId-$(VERSION).pom; \
		cp out/datomlite/$$platform/$(SCALA_VERSION)/jar.dest/out.jar $$dir/$$artifactId-$(VERSION).jar; \
		cp out/datomlite/$$platform/$(SCALA_VERSION)/sourceJar.dest/out.jar $$dir/$$artifactId-$(VERSION)-sources.jar; \
		cp out/datomlite/$$platform/$(SCALA_VERSION)/docJar.dest/out.jar $$dir/$$artifactId-$(VERSION)-javadoc.jar; \
		for f in $$dir/*; do \
			md5sum $$f | cut -d' ' -f1 > $$f.md5; \
			sha1sum $$f | cut -d' ' -f1 > $$f.sha1; \
			gpg --batch --yes -ab -u $(GPG_KEY) $$f; \
		done; \
		echo "Packaged $$artifactId"; \
	done
	@cd $(BUNDLE_DIR) && zip -r datomlite-$(VERSION)-bundle.zip xyz
	@rm -rf $(BUNDLE_DIR)/xyz
	@echo "\nBundle ready: $(BUNDLE_DIR)/datomlite-$(VERSION)-bundle.zip"

svg:
	@for f in pix/*.dot; do \
		out="$${f%.dot}.svg"; \
		echo "dot -Tsvg $$f > $$out"; \
		dot -Tsvg "$$f" > "$$out"; \
	done
