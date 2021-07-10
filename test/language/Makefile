.phony: build
build: clean erlang javascript

.phony: clean
clean:
	rm -rf target-*

.phony: erlang
erlang:
	cargo run -- compile-package --name language --src src --out target-erlang --target erlang
	cp src/*.erl target-erlang/
	cd target-erlang && erlc *.erl
	escript main_erlang.erl

.phony: javascript
javascript:
	cargo run -- compile-package --name language --src src --out target-javascript --target javascript
	cp src/*.js target-javascript/
	node main_javascript.js
