# FAQs

- [Why is the compiler written in Rust?](#why-is-the-compiler-written-in-rust)
- [Will Gleam have type classes?](#will-gleam-have-type-classes)
- [How is message passing typed?](#how-is-message-passing-typed)
- [How does Gleam compare to Alpaca?](#how-does-gleam-compare-to-alpaca)
- [Should I put Gleam in production?](#should-i-put-gleam-in-production)
- [Is it good?](#is-it-good)


## Why is the compiler written in Rust?

Prototype versions of the Gleam compiler was written in Erlang, but a switch was
made to Rust as the lack of static types was making refactoring a slow and
error prone process. A full Rust rewrite of the prototype resulted in the
removal of a lot of tech debt and bugs, and the performance boost is nice too!

One day Gleam may have a compiler written in Gleam, but for now we are focused
on developing other areas of the language such as libraries, tooling, and
documentation.


## Will Gleam have type classes?

Some form of ad-hoc polymorphism could be a good addition to the ergonomics of
the language, though what shape that may take is unclear. Type classes are one
option, OCaml style implicit modules are another, or perhaps it'll be
something else entirely.


## How is message passing typed?

Gleam currently doesn't currently have first class support for the BEAM's
concurrency primitives such as `receive`, `send`, and `spawn`. This is because
research is still ongoing as to the best way to apply a strong type system to
them while still enabling established OTP patterns. For now these primitives
should be used via the Erlang FFI, making them dynamically typed.

Many OTP patterns such as `gen_server` are functional in nature and don't
require direct use of these primitives so these behaviours can be implemented
in Gleam today.


## How does Gleam compare to Alpaca?

[alpaca]: https://github.com/alpaca-lang/alpaca

[Alpaca][alpaca] is similar to Gleam in that it is a statically typed language
for the Erlang VM that is inspired by the ML family of languages. It's a
wonderful project and we hope they are wildly successful!

Here's a non-exhaustive list of differences:

- Alpaca functions are auto-curried, Gleam's are not.
- Alpaca's unions can be untagged, with Gleam all variants in an enum need a
  name.
- Alpaca's compiler is written in Erlang, Gleam's is written in Rust.
- Alpaca's syntax is closer to ML family languages, Gleam's is closer to C
  family languages.
- Alpaca compiles to Core Erlang, Gleam compiles to regular Erlang.

Alpaca is great, check it out! :)


## Should I put Gleam in production?

Probably not. Gleam is a very young language and there may be all kinds of
problems and breaking changes down the line.

Having said that, the Erlang VM is extremely mature and well tested, and if
you decide to move away from Gleam the language you can compile your code to
Erlang and maintain that in future.


## Is it good?

Yes.
