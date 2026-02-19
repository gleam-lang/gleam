# TODO: migrate to Rust shell commands, possibly ./compiler-cli/src/fs/tests.rs
test:
	# clean build directory && build the project
	cargo run clean && cargo run build

	# fail if file exists
	@if find build -name "gleam@@compile.erl" | grep -q .; then \
		echo "Error: gleam@@compile.erl file(s) still exist after build"; \
		exit 1; \
	fi
