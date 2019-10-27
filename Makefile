#
# Goals to be specified by user
#

.PHONY: help
help:
	@cat $(MAKEFILE_LIST) | grep -E '^[a-zA-Z_-]+:.*?## .*$$' | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: build
build: book ## Build the compiler and the book
	cargo build --release

.PHONY: install
install: ## Build the Gleam compiler and place it on PATH
	cargo install --path . --force

.PHONY: book
book: docs/index.html ## Build the documentation

.PHONY: book-serve
book-serve: ## Run the book dev server
	cd book && mdbook serve --open --websocket-port 4200

.PHONY: test ## Run all tests
test:
	cargo test

.PHONY: test-gleam-watch
test-watch: ## Run compiler tests when files change
	watchexec -e rs,lalrpop "echo; cargo test; echo; echo"

# Debug print vars with `make print-VAR_NAME`
print-%: ; @echo $*=$($*)

#
# Files
#

docs/index.html: $(shell find book/src -type f)
	rm -fr docs
	cd book && mdbook build --dest-dir ../docs/
