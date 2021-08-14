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
	cd compiler-cli && cargo install --path . --force --locked

.PHONY: test
test: ## Run the compiler unit tests
	cargo test

.PHONY: language-test
language-test: ## Run the language integration tests for all targets
	cd test/language && make

.PHONY: javascript-prelude-test
javascript-prelude-test: ## Run the JavaScript prelude core tests
	cd test/javascript_prelude && npm test

.PHONY: javascript-prelude-test-watch
javascript-prelude-test-watch: ## Run the JavaScript prelude core tests
	watchexec "cd test/javascript_prelude && npm test"

.PHONY: test-watch
test-watch: ## Run compiler tests when files change
	watchexec -e rs,html,capnp "cargo test"

# Debug print vars with `make print-VAR_NAME`
print-%: ; @echo $*=$($*)
