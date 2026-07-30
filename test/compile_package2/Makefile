.PHONY: build
build:
	# Remove any previously compiled code
	rm -rf out
	cargo run -- compile-package --out out --target erlang --lib . --package .
	erl -pa out/ebin -noshell -eval "erlang:display(one:hello()),erlang:display(two:hello()),erlang:display('Elixir.Three':hello()),halt()"
