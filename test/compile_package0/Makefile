.phony: build
build:
	# Remove any previously compiled code
	rm -rf out
	cargo run -- compile-package --out out --target erlang --lib . --package .
	erl -pa out/ebin -noshell -eval "erlang:display(two:main()),erlang:display(three:test()),halt()"
