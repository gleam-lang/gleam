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
	cargo install --path . --force --locked

.PHONY: test ## Run all tests
test:
	cargo test

.PHONY: test-watch
test-watch: ## Run compiler tests when files change
	watchexec -e rs,lalrpop,html,capnp "cargo test"

# Debug print vars with `make print-VAR_NAME`
print-%: ; @echo $*=$($*)
