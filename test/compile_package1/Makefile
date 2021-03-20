.phony: build
build:
	# Remove any previously compiled code
	rm -rf out1 out2
	# Compile the first Gleam package
	cargo run -- compile-package --name one --src src1 --out out1
	# Compile the second Gleam package which depends on the first
	cargo run -- compile-package --name two --src src2 --out out2 --lib out1
	# Compile the Erlang
	cd out1 && erlc *.erl
	cd out2 && erlc *.erl
	# Run the code
	erl -pa out1 out2 -noshell -eval "erlang:display(two:main()),halt()"
