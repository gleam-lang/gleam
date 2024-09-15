.phony: build
build: clean erlang nodejs deno bun

.phony: clean
clean:
	rm -rf build

.phony: erlang
erlang:
	@echo test/subdir_ffi on Erlang
	cargo run --quiet -- test --target erlang

.phony: nodejs
nodejs:
	@echo test/subdir_ffi on JavaScript with Node
	cargo run --quiet -- test --target javascript --runtime nodejs

.phony: deno
deno:
	@echo test/subdir_ffi on JavaScript with Deno
	cargo run --quiet -- test --target javascript --runtime deno

.phony: bun
bun:
	@echo test/subdir_ffi on JavaScript with Bun
	cargo run --quiet -- test --target javascript --runtime bun
