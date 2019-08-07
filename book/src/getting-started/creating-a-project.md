# Creating a project

## Installing the rebar3 build tool

**Note**: Gleam's tooling is very young and in a state of flux. Expect rough
edges and breaking changes to come.

The Gleam compiler can build Gleam projects that are managed with the standard
Erlang build tool, rebar3. If you don't have rebar3 installed please [install
it now](https://www.rebar3.org/).

[rebar_gleam]: https://github.com/gleam-lang/rebar_gleam#installation

## Generating a project

Now a project can be generated like so:

```sh
gleam new my_fantastic_project
cd my_fantastic_project
```

You'll now have a project with this structure:

```
.
├── gleam.toml
├── LICENSE
├── README.md
├── rebar.config
├── src
│   ├── my_fantastic_project.app.src
│   └── my_fantastic_project.gleam
└── test
    └── my_fantastic_project_test.gleam

2 directories, 7 files
```

The project is managed and built using rebar3, the standard Erlang build tool.
Here are some commonly used commands rebar3 commands that you can use with
your new project:

```sh
# Run an interactive shell with your code loaded (Erlang syntax)
rebar3 shell

# Run the eunit tests
rebar3 eunit
```

More information can be found on the [rebar3 documentation website](https://www.rebar3.org/docs).


## What next?

Want to see some Gleam code? See the [example projects](./example-projects.html).

Looking learn the language? Check out the [language tour](../tour).

Need ideas for a project? We have a [list of libraries][libraries] that need
writing.

[libraries]: https://github.com/gleam-lang/suggestions/issues?q=is%3Aopen+is%3Aissue+label%3Aarea%3Alibraries
