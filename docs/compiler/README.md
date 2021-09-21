# Compiler documentation

Hello! Welcome to the documentation for the Gleam compiler. I hope you have fun
with the project!

<!-- vscode-markdown-toc -->
* [Project structure](#Projectstructure)
* [Compilation flow](#Compilationflow)
* [Testing](#Testing)
	* [Running the tests](#Runningthetests)
	* [Snapshot testing](#Snapshottesting)

<!-- vscode-markdown-toc-config
	numbering=false
	autoSave=true
	/vscode-markdown-toc-config -->
<!-- /vscode-markdown-toc -->

## <a name='Projectstructure'></a>Project structure

The project is made up of several Rust crates (projects):

- `compiler-core`: This project parses, analyses, and compiles Gleam projects.
  It is entirely pure and has no IO so that is provided by the other Rust crates
  that wrap this one.
- `compiler-cli`: A command line interface that wraps the core compiler and
  provides IO to files and to the console.
- `compiler-wasm`: A JavaScript interface to the core compiler via web assembly.
  Suitable for running in a web browser.

In addition to the Rust code there are these components:

- `Makefile`: A makefile that defines shortcut commands for common tasks when
  working in the Gleam codebase. Run `make help` to view them.
- `test`: A collection of (mostly) Gleam projects that serve as integration tests for the
  compiler.
- `deny.toml`: Configuration for the `cargo-deny` tool which is used to ensure
  that the Rust libraries the compiler depends on adhere to our expectations.
- `containers`: A collection of docker-files used to produce OCI containers for
  each Gleam release.
- `.github/workflows`: GitHub Actions workflow definitions, used to build, test,
  and release new versions of the project.
- `docs`: You're looking at it pal.

## <a name='Compilationflow'></a>Compilation flow

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

## <a name='Testing'></a>Testing 

We like automated tests! They're a great way to verify that the compiler is
doing what we expect it do as we make changes.

### <a name='Runningthetests'></a>Running the tests

- `make test`: Run all the tests. Worth doing before committing any changes.
- `make test-watch`: Run the Rust unit tests when files are saved.
- `make language-test`: Run the cross-platform language integration tests in
  `test/language`.
- `make language-test-watch`: Run said tests when files are saved.
- `make javascript-prelude-test`: Run the unit tests for the JavaScript code
  that implements the Gleam prelude when compiling to JavaScript.
- `make javascript-prelude-test-watch`: Run said tests when files are saved.

The `*-watch` commands require the [`watchexec`][watchexec] program to be
installed.

### <a name='Snapshottesting'></a>Snapshot testing

The compiler makes heavy use of snapshot testing using the
[`cargo-insta`][cargo-insta] tool.

[cargo-insta]: https://github.com/mitsuhiko/insta
[watchexec]: https://github.com/watchexec/watchexec

If you're not familiar with snapshot testing instead of writing an input and an
expected output (as in normal example based tests) you write only the input. The
snapshot testing tool can then be used to mark any new outputs as accepted and
saved into the repository as snap files. If an output for one of the tests
changes then it is considered a failed test and the programmer has the option to
either reject the new version (as it is an incorrect result) or accept it as the
new correct output.

This style of testing saves us huge amounts of time as manually updating all the
expected output when we make changes to the output format of the compiler or
error messaging is time consuming and very dull. With snapshot testing it takes
seconds.

```shell
# Run the tests
make test

# Interactively verify changes to the snapshots
cargo insta review
```
