.phony: build
build: clean erlang javascript

.phony: clean
clean:
	@rm -rf target-*

.phony: erlang
erlang:
	@echo test/language on Erlang
	@cargo run --quiet -- compile-package --package . --out target-erlang --target erlang --lib .
	@escript main_erlang.erl

.phony: javascript
javascript:
	@echo test/language on JavaScript
	@cargo run --quiet -- compile-package --package . --out target-javascript --target javascript --lib .
	@cp src/*.mjs target-javascript/dist
	@node main_javascript.js
