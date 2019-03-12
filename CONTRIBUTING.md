# Contributing to Gleam

Thanks for contributing to Gleam!

Before continuing please read our [code of conduct][code-of-conduct] which all
contributors are expected to adhere to.

[code-of-conduct]: https://github.com/lpil/gleam/blob/master/CODE_OF_CONDUCT.md


## Contributing bug reports

If you have found a bug in Gleam please check to see if there is an open
ticket for this problem on [our GitHub issue tracker][issues]. If you cannot
find an existing ticket for the bug please open a new one.

[issues]: https://github.com/lpil/gleam/issues

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
   master branch.


## The Gleam codebase

This code repository contains multiple projects including:

- `./gleam` - The Gleam compiler, written in Rust.
- `./gleam_stdlib` - The Gleam standard library, written in Gleam.
- `./book` - The Gleam book/website.

You can run all the tests in the root directory by running `make test`. You
can also run the tests for a specific project with `make test-$NAME`, for
example `make test-stdlib`.

The book development server can be run with `make book-serve`, and any changes
can be compiled into HTML in the `./docs` directory with `make book`.
