Introduction
============

Hi. I'm Louis Pilfold. I discovered Elixir and Erlang nearly two years ago,
and now I'd like to share with you some of the things I learnt along that
journey. Specifically I'd like to talk about linters, parsers, compilers and
the BEAM.

Prior to discovering Elixir I had been enthusiastically exploring Haskell, but
professionally I was writing Ruby. Perhaps due to the combination of the
functional style of problem solving and the Ruby-esc community Elixir
instantly resonated with me and quickly became the language I wanted to write
every day. With Haskell I was solving puzzles and coding challenges, but with
Elixir I found myself wanting to be constructive and productive, I wanted to
build tools.

Back in the Ruby world I had developed a soft spot for static analysis
programs. One such tool was the style linter Rubocop, which is a program that
inspects your codebase for style errors and common mistakes. Elixir being a
young language we didn't an equivilent tool, so I decided to take a shot at
making one myself.

```
Source code -> Errors
```

Linters are effectvely simple functions that take source code files as an
input, and return a number of errors to the user, so I figure it shouldn't be
too hard. I didn't know exactly how they worked so I looked at source for
Rubocop and a Javascript linter called JSCS and discovered that if you look a
little closer they look like this.


```
Source code -> Data structure -> Errors
```

They take source code, convert them to one or more different data structures,
and then analyse those forms to find any errors to present to the user which
would now be patterns in the data that we could match against.

```
Source code -> Tokens -> Errors
```

The first data structure we can create from source code is a list of tokens.
Tokens represents the smallest elemental parts of the source code we type, the
basic textual building blocks of code.

```
"Hello, world!"

- word "Hello"
- punctuation ","
- space " "
- word "world"
- punctuation "!"
```

For example, if we took the English sentence "Hello, world!" and tokenize it
we might end up with something like this.

We have 5 tokens, the first one is a word with a value of "Hello".
The second is a piece of punctuation with a value of a comma.
The third is a space.
The fourth is word with a value of "world".
And lastly we have another punctuation token with the value of an exclaimation mark.

```
"1 |> add 2"

- number 1
- arrow_op |>
- identifier add
- number 2
```

Here we can do the same with Elixir code. Here's a snippet of code in which I
pipe the number one into a function called "add" that takes an additional
variable of 2. When tokenized it becomes this a list of 4 tokens:

A number token with a value of 1.
An arrow_op token with a value of the characters that make up the pipe operator.
An identifier token of the value add.
And another number token with the value of 2.

```elixir
"1 |> add 2"

[{:number, 1},
 {:arrow_op, :|>},
 {:identifier, :add},
 {:number, 2}]
```

This data in Elixir terms would look like this. Each token is a tuple where the
first element is the name of the token type as an atom, and the last element
is the value of the token, so the atom "add", or the number 2.

In the Ruby and Javascript linters I looked at the Tokenization process was
quite long and complex, but as often the case I found Elixir did all the hard
work for me. As the Elixir compiler is written in Erlang we can reuse the
tokenizer for for language itself.

```elixir
iex(3)> :elixir_tokenizer.tokenize '1 |> add 2 ', [], []

{:ok, [], 12,
 [{:number, {[], 1, 2}, 1},
  {:arrow_op, {[], 3, 5}, :|>},
  {:identifier, {[], 6, 9}, :add},
  {:number, {[], 10, 11}, 2}]}
```

It's just a "tokenize" function in an Erlang module named "elixir_tokenizer"
which when given a char list of source code it gives us back tokens. These
tokens also contain a middle value of some metadata which I'm ignoring for
now.

That was easy. So how might these tokens be used in a linter?

```elixir
IO.puts("Hello"); # Bad
IO.puts("World")  # Good
```

One simple thing an Elixir linter might do is forbid the use of semicolons to
separate expressions. Each expression should instead be seperated by newlines,
as is more common.

```elixir
def semicolon?({:";", _, _}),
  do: true
def semicolon?(_),
  do: false

if tokens |> Enum.any?(&semicolon?/1) do
  :error
else
  :ok
end
```

To make the linter detect violations of this rule I first defined a function
called "semicolons?" (question mark). It returns true if passed a semicolon
token, and false otherwise, which it detects by pattern matching on the first
element of the token tuple, the type atom.

Now with this function I can just iterate over the list, and return an error
if any semicolon tokens are found. Here I'm just using the `any?/2` function,
in the linter I'd filter for the offending tokens and return a message to the
user for each one.

Tokens are the first data structure representation of source code that can be
analysed, a second (more useful) data structure is an abstract syntax tree.

```
"add 1, 2"

function_call add
  ├─ number 1
  └─ number 2
```

While tokens were the linear sequence of all the elemental components of source
code, an abstract syntax tree is a representation of the the syntactic
structure of the source code.

For example, this piece of code in which I call a function "add" with the
arguments 1 and 2 would result in the three pictured.

The root node is a call to the function "add".
This call node has 2 leaf node children.
The first is the number 1, the second is the number 2.

```
"send self(), {:compare 1, 2 + 2}"

function_call send
  ├─ function_call self
  └─ tuple
     ├─ atom compare
     ├─ function_call +
     │  ├─ number 2
     │  └─ number 2
     └─ number 1
```

Here's a more complex example.

At the root of the tree there's a call to the "send" function, which has 2
arguments, and thus 2 children. The first is a call to the zero arity function
"self", and the second is a tuple. The tuple has 3 children, the atom
"compare", a function call, and the number 1. The function call is to the plus
operator, and has 2 children, each the number 2.



------------------------------------------------------------------------------

                            More stuff to come...

------------------------------------------------------------------------------


```
Source code -> Tokens               -> Errors
               Abstract Syntax Tree -----^
```

The hard bit here is typically getting the intemediary forms, but in Elixir
there is have the `quote` special form, which gives us back the AST of any
code we pass to it.

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
