# glerm

[![Package Version](https://img.shields.io/hexpm/v/glerm)](https://hex.pm/packages/glerm)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/glerm/)

A Gleam wrapper around `crossterm` to create terminal applications

## Quick start

### Caveats

Currently, this only works on Linux. Gleam doesn't support any mechanism to
build the NIFs on your machine. To get around this for now, I am shipping
pre-compiled library files in the `priv/` directory.

The `.dll` for windows does actually allow the program to run. Most of the
methods seem to work, but unfortunately the blocking `event::read()` call
just hangs. I expect this is some interaction between `erlang`, `crossterm`,
and Windows. I may try to look more into this, but right now I don't really
know what the issue is. Would love some help with this if possible!

I tried to build this on my Intel Macbook, but got some errors from `rustler`.
I'm not sure if that's not considered a supported platform anymore by them,
but it's also currently not working. Any help with that would also be
appreciated.

I don't have access to anything ARM, so unfortunately will not be able to
provide anything in that regard.

### Getting started

The docs should hopefully be helpful. Additionally, there is at least one
example usage in `examples/` that you can run with `gleam run` (if you are
using a version of Gleam that supports path dependencies).

## Installation

If available on Hex this package can be added to your Gleam project:

```sh
gleam add glerm
```

and its documentation can be found at <https://hexdocs.pm/glerm>.
