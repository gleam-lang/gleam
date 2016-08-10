Introduction
============

Hi. I'm Louis Pilfold. I'd like to talk today a bit about my experience with
Elixir and some of the things I learnt about compilers and the BEAM along the
way.

I've been writing Elixir for about a year and a half, prior to that I had been
playing with Clojure and Haskell, and professionally I was writing Ruby.
Perhaps it's due to the combination of the functional style of problem solving
and the Ruby-esc community but Elixir instantly resonated with me and quickly
became the language I wanted to write every day.

Back in the Ruby world I had developed a soft spot for static analysis tools,
such as the style linter Rubocop. Elixir being a young language we didn't an
equivilent tool, so I decided to take a shot at making one myself. Linters
kind of look like this:

    Source code -> Abstract Syntax Tree -> Errors

    Source code -> Tokens -> Errors

They take source code, convert them to intemediary forms such as an abstract
syntax tree and tokens, and then analyse those forms to find any errors to
present to the user. The hard bit here is typically getting the intemediary
forms, but in Elixir we have the `quote` special form, which gives us back the
AST of any code we pass to it.

```elixir
iex(1)> quote do add(1, 2) end
{:add, [], [1, 2]}
```

In case you're not familiar, here's an example of the Elixir abstract syntax
tree. It's deliberately simple and uniform so that it's easy to read and
manipulate. Everything in the Elixir AST that isn't one of the primitive types
is a functional call, and a function call is a three item tuple. The first
item is the name of the function being called, which is an atom or an
expression. The second item in the tuple is a list of metadata that may
include line numbers or information on imports so that we can resolve the
function name if we need to. Lastly the third item in the tuple is a list of
arguments. Here I'm calling a fictional function called "add" with two
arguments, 1 and 2, so my AST has the atom `:add` as the name, and a list
containing 1 and 2 as arguments.

```elixir
iex(6)> Code.string_to_quoted "add(1, 2)"
{:ok, {:add, [line: 1], [1, 2]}}
```

And if we want to do the same with a string of code rather than an expression
we can use the `string_to_quoted` function in the Elixir `Code` module.

Great, so that's the AST tree generation handled and we didn't have to write a
single line. What about getting the tokens from source code? It turns out that
the Elixir standard library contains an Erlang module called
`:elixir_tokenizer`, which exposes a function that offers a way to get
those tokens from a source code string.

```elixir
iex(3)> :elixir_tokenizer.tokenize 'add(1, 2)', [], []
{:ok, [], 10,
 [{:paren_identifier, {[], 1, 4}, :add},
  {:"(", {[], 4, 5}},
  {:number, {[], 5, 6}, 1},
  {:",", {[], 6, 7}},
  {:number, {[], 8, 9}, 2},
  {:")", {[], 9, 10}}]}
```

Here's the function in action. It's certainly not one of the modules you're
encouraged to use by the Elixir core team, and it's maybe a little unstable as
in a previous version of Elixir the tokens returned were subtly different for
some inputs, but it's good enough for my use case here.

TODO: Talk about the format.

After this it's just a matter of pattern matching on these forms to detect
errors. Simple.

TODO: Give an example of usage. Possibly with function arity.

If anyone is interested in this project it can be found on my GitHub account
under the name `Dogma`.

---

- Talk about Jot
  - Parsing
  - Transforming and optimising the data structure
  - Outputting Elixir AST

- Talk about mix test.watch
  - Reloading modules
  - Reading the internals of the compiler, etc

- Talk about how together all these skills kind of look like a compiler. Could
  I make a compiler?
