# Compiler documentation

Hello! Welcome to the documentation for the Gleam compiler. I hope you have fun
with the project!

There is currently no formal specification of Gleam. See the the
programmer-oriented [Language tour][language-tour] and the [language integration
tests][language-tests] for information on the language itself.

[language-tour]: https://gleam.run/book/tour/index.html
[language-tests]: https://github.com/gleam-lang/gleam/tree/main/test/language

<!-- vscode-markdown-toc -->
* [Project structure](#Projectstructure)
* [Compilation flow](#Compilationflow)

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
- `test`: A collection of Gleam projects that serve as integration tests for the
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
