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

## Local development

To run the compiler tests.

```shell
cargo test
```

If you don't have Rust or Cargo installed you can run the above command in a docker sandbox.
Run the command below from this directory.

```shell
docker run -v $(pwd):/opt/app -it -w /opt/app rust:1.44.0 bash
```

## Rust development

Here are some tips and guidelines for writing Rust code in the Gleam compiler:

Never write code that can cause the compiler to panic (`panic!`, `unwrap`,
`expect`) as a compiler panic is confusing to the user. When possible rewrite
the code in a way that makes the error impossible. If that cannot be done and
the error is either common or due to a mistake by the user return an error
value that will be printed with an appropriate helpful error message. If the
error _should_ never happen and its occurrence indicates a fatal compiler bug
the `.gleam_expect` method of the `GleamExpect` trait can be used. This is
similar to `.expect` but prints a more helpful error message to the user.

The `GLEAM_LOG` environment variable can be used to cause the compiler to
print more information for debugging and introspection. i.e.
`GLEAM_LOG=trace`.
