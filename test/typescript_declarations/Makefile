.PHONY: clean
clean:
	rm -rf build

.PHONY: build
test:
	cargo run --quiet -- build
	tsc ./build/dev/javascript/typescript_declarations/typescript_declarations.d.mts --noEmit --lib es2015

