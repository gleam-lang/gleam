.phony: build
build: clean erlang javascript

.phony: clean
clean:
	@rm -rf target-*

.phony: erlang
erlang:
	@echo test/language on Erlang
	@cargo run --quiet -- test --target erlang

.phony: javascript
javascript:
	@echo test/language on JavaScript with Node
	@cargo run --quiet -- test --target javascript --runtime nodejs

.phony: deno
deno:
	@echo test/language on JavaScript with Deno
	@cargo run --quiet -- test --target javascript --runtime deno
