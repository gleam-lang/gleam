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
2. Update the [CHANGELOG.md](CHANGELOG.md) file with your changes.
3. Open a GitHub pull request with your changes and ensure the tests and build
   pass on CI.
4. A Gleam team member will review the changes and may provide feedback to
   work on. Depending on the change there may be multiple rounds of feedback.
5. Once the changes have been approved the code will be rebased into the
   `main` branch.

## Local development

To run the compiler tests. This will require a recent stable version of Rust,
Erlang, Elixir, NodeJS, Deno, and Bun to be installed.

If you are using the Nix package manager, there's a [gleam-nix flake](https://github.com/vic/gleam-nix)
you can use for running any Gleam version or quickly obtaining a development
environment for Gleam.

```sh
cargo test

# Or if you have watchexec installed you can run them automatically
# when files change
make test-watch
```

To run the language integration tests. This will require a recent stable
version of Rust, Erlang, and NodeJS to be installed.

```sh
make language-test
```

If you don't have Rust or Cargo installed you can run the above command in a
docker sandbox. Run the command below from this directory.

```sh
docker run -v $(pwd):/opt/app -it -w /opt/app rust:latest bash
```

## Rust development

Here are some tips and guidelines for writing Rust code in the Gleam compiler:

The `GLEAM_LOG` environment variable can be used to cause the compiler to
print more information for debugging and introspection. i.e.
`GLEAM_LOG=trace`.

### Clippy linter

Your PR may fail on CI due to clippy errors. Clippy can be run locally like so:

```sh
cargo clean -p gleam
cargo clippy
```

If you have lint errors on CI but not locally upgrade your Rust version to the
latest stable.

```sh
rustup upgrade stable
```

## Operating system specific code

This project is used on FreeBSD, Linux, MacOS, OpenBSD, Windows, and presumably
other operating systems, so there is some amount of code that needs to be
different depending on which is it running on. So far this is hidden inside
dependencies, with the exception of some code for working with file paths in
tests and for setting file permissions, which is different on Windows. If you
are working in this area then you may get a CI failure relating to this for
your first attempt. If you need help resolving any issues do not hesitate to
ask.

## Using large language models

Large language models are increasingly being used by developers in day-to-day
programming as they can make certain tasks much easier. They also present some
problems. We believe that most AI-generated contributions are done in good faith,
with a genuine intention to help the project, however without proper care they
can create issues for the project's maintainers.

### Using LLMs to write code

The main issue is that generating some code with AI and opening a pull request
is exceptionally easy to do, and requires little to no effort from the person
using the LLM. This extremely low bar puts extra strain on the maintainers, who
have to review the code, which is coming in at a higher rate than human-written
contributions, and that may not have been looked over by a human before.

The second issue is the onboarding of new contributors. The `gleam-lang/gleam`
repository has a number of issues tagged as "Good first issue", which generally
require only a small set of changes. These are useful for new contributors to
familiarise themselves with the codebase, allowing them to build up to working
on more complicated issues in future. When new contributors use LLMs to solve
"Good first issue"s, they don't learn anything and the purpose of these is
defeated.

### Using LLMs to write text

Another common use for LLMs is to write summaries of changes for pull request
descriptions. LLMs tend to write overly lengthy descriptions, which contain
unnecessary details and waste the time of maintainers who need to read through
everything. For PR descriptions, consider that:
- The maintainers are familiar with the codebase, and should be able to understand
  any reasonably straightforward code without assistance.
- For any code that is not self-explanatory or is confusing, an inline comment is
  vastly preferred to a comment in the pull request. The maintainer will see it
  in the diff, and future contributors will be able to refer to it without needing
  to dig up the pull request where the code was originally added.

We have also observed people using LLMs to reply to issues and discussions to
provide potential solutions or alternative proposals. The problem here is twofold:
firstly, AI often does not understand the codebase enough to propose a solution
which actually makes sense, and secondly, issues and discussions are about
allowing humans to discuss their opinions and suggestions on Gleam features. When
people use AI, it becomes difficult for others to have a reasonable conversation
with them, as there is no human connection.

### Our LLM policy

In all of the above situations, consider that if others wanted the input of AI
on a problem, they can ask it directly. Copy-pasting the unedited output of an
LLM into a discussion or pull request adds little to no value to the conversation.
On the other hand, providing your human experience and opinion can be very helpful.

Any pull request, issue, or discussion is an exchange of time. The contributor
spends their time writing up an issue, providing their opinion, or implementing
a code change. The maintainer responds by spending their time triaging the issue
or reviewing the code. As a result, the project benefits. This exchange of time
ensures fairness and empathy. Since both parties are spending their time, it
ensures neither person's effort is wasted. When using AI to generate code or
text, this removes the time exchange. The "contributor" spends a small amount
of effort prompting an LLM, but requires the maintainer to spend the same amount
of (or often more) time, which pushes extra responsibility and work onto the
maintainer.

With the above said, please do not:
1. Open pull requests using LLMs entirely to fix issues tagged as "Good first
  issue".
2. Open pull requests containing code entirely generated by LLMs without first
  reviewing the code changes and ensuring it works, making any necessary changes
  as you see fit.
3. Copy-paste AI-generated passages of text into pull request descriptions, comments,
  or Github issues/discussions, without reviewing them first and cutting out
  unnecessary details.

Feel free to:
1. Use AI to help you understand parts of the codebase before writing code
  yourself.
2. Use LLMs to help you write code, provided you review the code that is generated,
  and ensure it does what you expect.

At the end of the day, AI is a tool, not a replacement for human thought. If
you use it, do so responsibly, in a way that avoids pushing burdensome effort
onto the maintainers.
