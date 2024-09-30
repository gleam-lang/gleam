# Compiler WASM

```shell
# Install the build tool with cargo or brew etc
cargo install wasm-pack

# Build the wasm library
wasm-pack build --release --target web

# Make a tarball to attach to a release
tar -C pkg/ -czvf gleam-v1.1.0-browser.tar.gz .
```
