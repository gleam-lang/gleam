.PHONY: build
build:
	# Remove any previously compiled code
	rm -rf out
	cargo run -- compile-package --out out --target erlang --lib . --package .
	exit 3
