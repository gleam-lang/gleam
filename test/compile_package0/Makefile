.phony: build
build:
	# Remove any previously compiled code
	rm -rf out
	# Compile the Gleam
	cargo run -- compile-package --name some_name --src src --test test --out out
	# Compile the Erlang
	cd out && erlc *.erl
	# Run the code
	erl -pa out -noshell -eval "erlang:display(two:main()),erlang:display(three:test()),halt()"
