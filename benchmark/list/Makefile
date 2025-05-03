.PHONY: build
build: clean erlang nodejs deno bun

.PHONY: clean
clean:
	rm -rf build

.PHONY: erlang
erlang:
	@echo test/language on Erlang
	cargo run --quiet -- test --target erlang

.PHONY: nodejs
nodejs:
	@echo test/language on JavaScript with Node
	cargo run --quiet -- test --target javascript --runtime nodejs

.PHONY: deno
deno:
	@echo test/language on JavaScript with Deno
	cargo run --quiet -- test --target javascript --runtime deno

.PHONY: bun
bun:
	@echo test/language on JavaScript with Bun
	cargo run --quiet -- test --target javascript --runtime bun
