# Compiler WASM

```shell
# Install the build tool
cargo install wasm-pack

# Build the library
wasm-pack build

# Or, build and copy to a directory
wasm-pack build --out-dir /home/${USER}/projects/gleam-playground/gleam-wasm
```

Run tests using `node` with the compiled WebAssembly.

```shell
wasm-pack test --node
```
