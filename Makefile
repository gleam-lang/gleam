COMPILER=$(realpath gleam/target/release/gleam)

#
# Goals to be specified by user
#

.PHONY: build
build: book gleam gleam_stdlib/gen gleam_decode/gen ## Build all targets

.PHONY: install
install: gleam ## Build the Gleam compiler and place it on PATH
	cd gleam && cargo install --path . --force

.PHONY: help
help:
	@cat $(MAKEFILE_LIST) | grep -E '^[a-zA-Z_-]+:.*?## .*$$' | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: gleam
gleam: gleam/target/release/gleam ## Build the Gleam compiler

.PHONY: book
book: docs/index.html ## Build the documentation

.PHONY: book-serve
book-serve: ## Run the book dev server
	cd book && mdbook serve --open

.PHONY: test ## Run all tests
test: test-gleam test-stdlib

.PHONY: test-gleam
test-gleam: ## Test the compiler
	cd gleam && cargo test

.PHONY: test-stdlib
test-stdlib: $(COMPILER) gleam_stdlib/gen ## Test gleam_stdlib
	cd gleam_stdlib && rebar3 eunit

# Debug print vars with `make print-VAR_NAME`
print-%: ; @echo $*=$($*)

#
# Files
#

gleam_stdlib/gen: $(COMPILER) $(shell find gleam_stdlib -type f)
	rm -fr gleam_stdlib/gen
	$(COMPILER) build gleam_stdlib

gleam_decode/gen: $(COMPILER) $(shell find gleam_decode -type f)
	rm -fr gleam_decode/gen
	$(COMPILER) build gleam_decode

$(COMPILER): gleam/Cargo.toml gleam/Cargo.lock gleam/build.rs $(shell find gleam/src -type f)
	cd gleam && cargo build --release

docs/index.html: $(shell find book/src -type f)
	rm -fr docs
	cd book && mdbook build --dest-dir ../docs/
