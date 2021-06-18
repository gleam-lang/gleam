# Architecture

This document describes the high-level architecture of the Gleam transpiler.

## Overview

On the highest level, a `gleam` binary receives Gleam source code as input
and produces one of its available backend formats:

- Erlang source code (not Erlang Abstract Format, as e.g. Elixir does)
- Javascript source code

There is currently no formal specification of Gleam source,
apart from the programmer-oriented [Language tour](https://gleam.run/book/tour/index.html).

The code transpilation is for the most part oriented around the properties
provided by Erlang's BEAM machine, including:

- immutable-only data structures
- dynamic types on Erlang's side
- functional programming

This unavoidably influences other backends, although Gleam strives to provide feature completeness.

## Code map

This section describes various modules comprising Gleam's core structure.

Pay attention especially to the following sections:

- **Architecture Invariant**: important note about crucial assumptions about the code/behavior
- **API Boundary**: an exposed interface that requires careful design, e.g. because it has internal users to support

### `parse`

TODO

### `erl`

TODO

### `javascript`

TODO
