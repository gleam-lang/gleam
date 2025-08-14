.PHONY: clean
clean:
	rm -rf build

.PHONY: test
test:
	cargo run --quiet -- build
	bunx tsc ./main.ts --strict --noEmit --skipLibCheck false --lib es2020,dom
