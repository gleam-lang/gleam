.PHONY: clean
clean:
	rm -rf build

.PHONY: test
test:
	cargo run --quiet -- build
	npx tsc ./main.ts --strict --noEmit --lib es2015,dom
