# Gleam Erlang 🐙

A library for making use of Erlang specific code!

## Features

- Typed Erlang processes and message sending.
- Erlang binary format (de)serialisation.
- Functions for working with Erlang's charlists.
- Reading, writing, and deletion of files.
- Basic distributed Erlang support and working with nodes.
- Reading and writing of environment variables.
- Functions for working with atoms.

## Usage

Add this library to your Gleam project

```shell
gleam add gleam_erlang
```

And then use it in your code

```gleam
import gleam/io
import gleam/erlang/file

pub fn main() {
  assert Ok(contents) = file.read("pokedex.txt")
  io.println(contents)
}
```

Documentation can be found at <https://hexdocs.pm/gleam_erlang/>.

This library requires OTP 23.0 or higher.
