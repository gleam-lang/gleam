# TODO: migrate to Rust shell commands, possibly ./compiler-cli/src/fs/tests.rs
test:
	# remove old tarball && create one && make will fail when it wasn't
	cargo run clean && cargo run export hex-tarball && make build/hextarball-0.1.0.tar
