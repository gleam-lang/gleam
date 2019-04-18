# Creating a project

## Installing the rebar3 build tool

**Note**: Gleam's tooling is very young and in a state of flux. Expect rough
edges and breaking changes to come.

The Gleam compiler can build Gleam projects that are managed with the standard
Erlang build tool, rebar3. If you don't have rebar3 installed please [install
it now](https://www.rebar3.org/) and the [rebar_gleam][rebar_gleam] plugin.

[rebar_gleam]: https://github.com/gleam-lang/rebar_gleam#installation

## Generating a project

Now a project can be generated using rebar3 like so:

```sh
rebar3 new gleam_lib my_fantastic_project
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

Here are some commonly used commands rebar3 commands that you can use with
your new project:

```sh
# Run an interactive shell with your code loaded (Erlang syntax)
rebar3 shell

# Run the eunit tests
rebar3 eunit
```

More information can be found on the [rebar3 documentation website](https://www.rebar3.org/docs).
