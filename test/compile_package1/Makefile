.phony: build
build:
	rm -rf out1 out2
	cargo run -- compile-package --in app1 --out out1
	cargo run -- compile-package --in app2 --out out2 --lib .
	erl -pa out1/ebin out2/ebin -noshell -eval "erlang:display(two:main()),halt()"
