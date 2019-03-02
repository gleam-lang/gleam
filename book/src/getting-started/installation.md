# Installation

A Gleam installation is made up of two parts: The Gleam compiler, and the
Erlang runtime.

## Installing the compiler

The compiler is written in the Rust programming language and must be build
from source. [Install the Rust compiler](https://www.rust-lang.org/tools/install)
before proceeding.

```sh
# Download the Gleam source code git reponsitory
cd /tmp
git clone https://github.com/lpil/gleam.git

# Build the Gleam compiler. This will take some time!
cd gleam/gleam
cargo install --path . --force

# Verify the compiler is installed
# Prints "gleam 0.1.0"
gleam --version
```

## Installing the Erlang runtime

Gleam compiles to Erlang code, so Erlang needs to be installed to run Gleam
code.

TODO: details on installing Erlang.

### Linux

### Mac OS X

### Windows

### Using version managers
