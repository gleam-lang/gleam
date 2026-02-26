#
# Goals to be specified by user
#

.PHONY: help
help:
	@cat $(MAKEFILE_LIST) | grep -E '^[a-zA-Z_-]+:.*?## .*$$' | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: build
build: ## Build the compiler
	cargo build --release

.PHONY: install
install: ## Build the Gleam compiler and place it on PATH
	cd gleam-bin && cargo install --path . --force --locked

.PHONY: test
test: ## Run the compiler unit tests
	cargo test --quiet
	cargo clippy
	cd test/language && make
	cd test/javascript_prelude && make test
	cd test/project_erlang && cargo run clean && cargo run check && cargo run test
	cd test/project_javascript && cargo run clean && cargo run check && cargo run test
	cd test/project_deno && cargo run clean && cargo run check && cargo run test
	cd test/hextarball && make test
	cd test/running_modules && make test
	cd test/subdir_ffi && make

.PHONY: insta-check-unused
insta-check-unused: ## Check for unused cargo insta snapshots
	cargo insta test --unreferenced=warn

.PHONY: insta-fix-unused
insta-fix-unused: ## Remove unused cargo insta snapshots
	cargo insta test --unreferenced=delete

.PHONY: language-test
language-test: ## Run the language integration tests for all targets
	cd test/language && make

.PHONY: language-test-watch
language-test-watch: ## Run the language integration tests for all targets when files change
	watchexec "cd test/language && make"

.PHONY: javascript-prelude-test
javascript-prelude-test: ## Run the JavaScript prelude core tests
	cd test/javascript_prelude && make test

.PHONY: javascript-prelude-test-watch
javascript-prelude-test-watch: ## Run the JavaScript prelude core tests when files change
	watchexec "cd test/javascript_prelude && make test"

.PHONY: test-watch
test-watch: ## Run compiler tests when files change
	watchexec -e rs,toml,gleam,html,capnp "cargo test --quiet"

.PHONY: export-hex-tarball-test
export-hex-tarball-test: ## Run `gleam export hex-tarball` and verify it is created
	cd test/hextarball && make test

.PHONY: benchmark
benchmark: ## Run the benchmarks
	cd benchmark/list && make

# Debug print vars with `make print-VAR_NAME`
print-%: ; @echo $*=$($*)
