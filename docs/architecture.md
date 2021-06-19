# Architecture

This document describes the high-level architecture of the Gleam compiler.

## Overview

On the highest level, a `gleam` binary receives Gleam source code as input
and produces one of its available backend formats:

- Erlang source code.
- JavaScript source code.

There is currently no formal specification of Gleam source, apart from the
programmer-oriented [Language tour][language-tour] and the [language integration
tests][language-tests].

[language-tour]: https://gleam.run/book/tour/index.html
[language-tests]: https://github.com/gleam-lang/gleam/tree/main/test/language

## Compilation flow

The process for compiling Gleam modules within the compiler looks roughly like
this:

```text
  Gleam source code       .gleam_module binaries
          ▼                         ▼
┌────────────────────┐ ┌───────────────────────┐
│       Parser       │ │ Metadata deserializer │
└────────────────────┘ └───────────────────────┘
          │                         │
      Untyped AST            Module metadata
          └─────────┐   ┌────────┘     │
                    ▼   ▼              │
           ┌─────────────────────┐     │
           │  Dependency sorter  │     │
           └─────────────────────┘     │
                      │                │
                 Untyped AST           │
              (sorted by deps)         │
                      ▼                │
            ┌───────────────────┐      │
            │   Type checker    │◄─────┘
            └───────────────────┘
                      │
          ┌────── Typed AST ──────┐
          ▼                       ▼
┌────────────────────┐ ┌─────────────────────┐
│   Code generator   │ │ Metadata serializer │
└────────────────────┘ └─────────────────────┘
          │                       │
          │                       ▼
 Erlang or JavaScript   .gleam_module binaries
   printing algebra
          ▼
┌────────────────────┐
│   Pretty printer   │
└────────────────────┘
          │
          ▼
 Erlang or JavaScript 
     source code
```

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
