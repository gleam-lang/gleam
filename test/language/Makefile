.phony: build
build: clean erlang javascript

.phony: clean
clean:
	@rm -rf target-*

.phony: erlang
erlang:
	@echo test/language on Erlang
	@cargo run --quiet -- compile-package --in . --out target-erlang --target erlang
	@escript main_erlang.erl

.phony: javascript
javascript:
	@echo test/language on JavaScript
	@cargo run --quiet -- compile-package --in . --out target-javascript --target javascript
	@cp src/*.js target-javascript/dist
	@node main_javascript.js
