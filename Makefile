.PHONY: compile test test-jvm test-js test-native publish-local clean fmt fmt-check repl svg

VERSION := 0.0.1

compile:
	./mill datomlite.__.compile

repl:
	./mill -i datomlite.jvm[3.7.3].console

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

svg:
	@for f in pix/*.dot; do \
		out="$${f%.dot}.svg"; \
		echo "dot -Tsvg $$f > $$out"; \
		dot -Tsvg "$$f" > "$$out"; \
	done
