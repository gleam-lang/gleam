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

```
Source code -> Abstract Syntax Tree -> Errors

Source code -> Tokens -> Errors
```

They take source code, convert them to intemediary forms such as an abstract
syntax tree and tokens, and then analyse those forms to find any errors to
present to the user. The hard bit here is typically getting the intemediary
forms, but in Elixir we have the `quote` special form, which gives us back the
AST of any code we pass to it.

```elixir
iex(1)> quote do add 1, 2  end
{:add, [], [1, 2]}
```

In case you're not familiar, here's an example of the Elixir abstract syntax
tree (or AST). It's deliberately simple and uniform so that it's easy to read
and manipulate. Everything in the Elixir AST that isn't one of the primitive
types is a functional call, and a function call is a three item tuple. The
first item is the name of the function being called, which is an atom or an
expression. The second item in the tuple is a list of metadata that may
include line numbers or information on imports so that we can resolve the
function name if we need to. Lastly the third item in the tuple is a list of
arguments. Here I'm calling a fictional function called "add" with two
arguments, 1 and 2, so my AST has the atom `:add` as the name, and a list
containing 1 and 2 as arguments.

```elixir
iex(6)> Code.string_to_quoted "add 1, 2"
{:ok, {:add, [line: 1], [1, 2]}}
```

And if we want to do the same with a string of code rather than an expression
we can use the `string_to_quoted` function in the Elixir `Code` module.

After this it's just a matter of pattern matching on these forms to detect
errors.

TODO: Give an example of usage. Unless else.
      Can show how everything in Elixir can be expressed using this highly
      regular function call syntax.

So there's an easy way to get the AST, but what about getting the tokens? It
turns out that the Elixir standard library contains an Erlang module called
`:elixir_tokenizer`, which exposes a function that offers a way to get
those tokens from a source code string.


```elixir
iex(3)> :elixir_tokenizer.tokenize 'add 1, 2 ', [], []
{:ok, [], 10,
 [{:identifier, {[], 1, 4}, :add},
  {:number, {[], 5, 6}, 1},
  {:",", {[], 6, 7}},
  {:number, {[], 8, 9}, 2},
```

Here's the function in action. It's certainly not one of the modules you're
encouraged to use by the Elixir core team, and it's maybe a little unstable as
in a previous version of Elixir the tokens returned were subtly different for
some inputs, but it's good enough for my use case here.

The format is a little less regular than that of the Elixir AST as it's not
something Elixir developers are expected to encounter when using the language.
Like with the AST each item is a tuple, though this time it is a flat list of
tuples rather than a nested tree structure. The first element is the type of
token. Here we have the token types of `identifier`, number and comma. The
second element in the tuple is just some information about where the token is
in the source code, and then after that we have optional fields for additional
data. For example here with the `identifier` token we have "add", the
identifier name as a third value.

```elixir
IO.puts("Hello"); # Bad
IO.puts("World")  # Good
```

```elixir
is_semicolon = fn(t) -> elem(t, 0) == :";" end

if tokens |> Enum.any?(is_semicolon) do
  :error
else
  :ok
end

```

If in the linter I wanted to ban use of semicolons I could do it by iterating
over the list of tokens and returning an error if we find any semicolons
tokens. This is really simple example, but one that can't be done by
inspecting the AST.

As we can see, in Elixir it's trivial to access and study both the AST and the
tokens that make up Elixir. Building a linter is just a simple matter of
pattern matching on them, and then doing some plumbing so that errors are
presented to the user in a tasteful fashion. Easy. If you would like to see
how the linter project turned out it can be found on GitHub and Hex under the
name "Dogma".

```html
<!DOCTYPE html>
<html>
  <head>
    <title>
      Build Your Own Elixir
    </title>
    link
  </head>
  <body>
    <h1 id="conf">
      An Elixir LDN talk
    </h1>
  </body>
</html>
```

```pug
html
  head
    title Build Your Own Elixir
  body
    h1#conf An Elixir LDN talk
```

After a while of writing tools and libraries in Elixir I found myself needing
to make a fairly standard web application that generated web pages. Naturally
I thought I'd use Elixir, and try out the Phoenix framework. Pretty much
immediately I realised that I'd be spoilt by the HTML templating libraries of
Ruby and Javascript. Instead of having to write HTML and all its angle
brackets I could write something more lightweight as above, and then get back
to writing real code as soon as possible.

In the end this bugged me enough to want to have a go at making a templating
language like this for Elixir.

```
Template -> HTML AST -> Elixir function
```
```
Data -> Elixir function -> HTML string
```

The compilation process of a HTML templating library would look something like
this. It takes a template, parses it into an AST that represents some HTML,
and from that builds a super fast Elixir function. That function takes some
arbitrary Elixir data, and then outputs a string of HTML.

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
