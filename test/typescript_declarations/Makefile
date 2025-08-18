.PHONY: clean
clean:
	rm -rf build

.PHONY: test
test:
	cargo run --quiet -- build
	tsc ./main.ts --noEmit --lib es2015,dom
