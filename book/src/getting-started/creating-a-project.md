# Creating a project

**Note**: Gleam's tooling is very young and in a state of flux. Expect rough
edges and breaking changes to come.

The Gleam compiler can build Gleam projects that are managed with the standard
Erlang build tool, rebar3. If you don't have rebar3 installed please [install
it now](https://www.rebar3.org/).

In future we will be able to generate a pre-configured rebar3 project capable
of compiling Gleam, but for now it is a multi-step process.

Create the rebar3 project:

```sh
cd path/to/my/projects

# Run either...
rebar3 new app my_amazing_project_name # for an application
rebar3 new lib my_amazing_project_name # for a library

cd my_amazing_project_name
```

The Gleam compiler needs to know the name of the project to correctly compile
it, so let it know via a config file.

```sh
echo 'name = "my_amazing_project_name"' > gleam.toml
```

Gleam source code is compiled into Erlang source code, so we need to tell
rebar3 where to find this generated code.

```sh
printf '\n{src_dirs, ["src", "gen/src"]}.\n' >> rebar.config
printf '\n{profiles, [\n    {test, [{src_dirs, ["test", "gen/test"]}]}\n]}.\n' >> rebar.config
```

Rebar3 needs to know to run the Gleam compiler before it compiles any Erlang
code.

```sh
printf '\n{pre_hooks, [{compile, "gleam build ."}]}.\n' >> rebar.config
```

Lastly, create a directory to keep your Gleam tests.

```sh
mkdir test
```

And now your project is ready! Appliation code goes in `src/some_name.gleam`
and test code goes in `test/another_name.gleam`.

If you're not familiar with rebar3 here are some commonly used commands. More
information can be found on the [rebar3 documentation website](https://www.rebar3.org/docs).

```sh
# Run an interactive shell with your code loaded (Erlang syntax)
rebar3 shell

# Run any eunit compatible tests
rebar3 eunit
```

For a standard library consider adding `gleam_experimental_stdlib` to your
rebar3 deps.
