.phony: build
build:
	rm -rf out1 out2
	cargo run -- compile-package --package app1 --target erlang --out out2 --lib .
	cargo run -- compile-package --package app2 --target erlang --out out2 --lib .
	erl -pa out1/ebin out2/ebin -noshell -eval "erlang:display(two:main()),halt()"
