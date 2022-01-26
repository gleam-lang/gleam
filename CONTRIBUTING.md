# Contributing to Gleam

Thanks for contributing to Gleam!

Before continuing please read our [code of conduct][code-of-conduct] which all
contributors are expected to adhere to.

[code-of-conduct]: https://github.com/gleam-lang/gleam/blob/main/CODE_OF_CONDUCT.md


## Contributing bug reports

If you have found a bug in Gleam please check to see if there is an open
ticket for this problem on [our GitHub issue tracker][issues]. If you cannot
find an existing ticket for the bug please open a new one.

[issues]: https://github.com/gleam-lang/gleam/issues

A bug may be a technical problem such as a compiler crash or an incorrect
return value from a library function, or a user experience issue such as
unclear or absent documentation. If you are unsure if your problem is a bug
please open a ticket and we will work it out together.


## Contributing code changes

Before working on code it is suggested that you read the
[docs/compiler/README.md](docs/compiler/README.md) file.
It outlines fundamental components and design of this project.

Code changes to Gleam are welcomed via the process below.

1. Find or open a GitHub issue relevant to the change you wish to make and
   comment saying that you wish to work on this issue. If the change
   introduces new functionality or behaviour this would be a good time to
   discuss the details of the change to ensure we are in agreement as to how
   the new functionality should work.
2. Open a GitHub pull request with your changes and ensure the tests and build
   pass on CI.
3. A Gleam team member will review the changes and may provide feedback to
   work on. Depending on the change there may be multiple rounds of feedback.
4. Once the changes have been approved the code will be rebased into the
   `main` branch.

## Hacking on Gleam in Gitpod

If you have a web browser, you can get a fully pre-configured Gleam development environment in one click:

[![Open in Gitpod](https://gitpod.io/button/open-in-gitpod.svg)](https://gitpod.io/#https://github.com/gleam-lang/gleam)

## Local development

To run the compiler tests. This will require a recent stable version of Rust
to be installed.

If you are using the Nix package manager, there's a [gleam-nix flake](https://github.com/vic/gleam-nix)
you can use for running any Gleam version or quickly obtaining a development environment for Gleam.

```shell
cargo test

# Or if you have watchexec installed you can run them automatically 
# when files change
make test-watch
```

To run the language integration tests. This will require a recent stable
version of Rust, Erlang, and NodeJS to be installed.

```shell
make language-test
```

If you don't have Rust or Cargo installed you can run the above command in a docker sandbox.
Run the command below from this directory.

```shell
docker run -v $(pwd):/opt/app -it -w /opt/app rust:latest bash
```

## Rust development

Here are some tips and guidelines for writing Rust code in the Gleam compiler:

The `GLEAM_LOG` environment variable can be used to cause the compiler to
print more information for debugging and introspection. i.e.
`GLEAM_LOG=trace`.

### Clippy linter

Your PR may fail on CI due to clippy errors. Clippy can be run locally like so:

```shell
cargo clean -p gleam
cargo clippy
```

If you have lint errors on CI but not locally upgrade your Rust version to the
latest stable.

```shell
rustup upgrade stable
```

## Cap'n Proto schema

The compiler uses a Cap'n Proto schema to serialize/deserialize module information.
Occasionally, the schema needs to change. After modifying `compiler-core/schema.capnp`
you need to to re-generate `compiler-core/generated/schema_capnp.rs`. To do that,
[install Cap'n Proto](https://capnproto.org/install.html) and un-comment appropriate lines
in `compiler-core/build.rs`. Then you should be able to re-generate that file with:

```shell
cd compiler-core
cargo build
```
