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

```elixir
quote do
  add 1, 2
end

{:add, [], [1, 2]}
```

```
Code.string_to_quoted "add 1, 2"

{:ok, {:add, [line: 1], [1, 2]}}
```

Getting the abstract syntax tree of Elixir code is even easier than getting
the tokens thanks to its Lisp style macro system. If we want to get the AST
from an expression we can just pass it to the `quote` special form, or if it's
a string we can use `Code.string_to_quoted`.

Elixir's AST is consise and simple. Everything that is not a literal in the
AST is a three item tuple where the first element is the name of the function
or constructor, the second item is some metadata, and the third item is a list
of that node's children.

Here is the add function again. The root is a function call, so it's a three
item tuple. The first element is the atom "add" as that is the name of the
function, and it's children are the literal numbers 1 and 2 in the third
position. This is a nice AST to work with, it's relatively readable and can be
pattern matched on easily.

```elixir
# Forbidden expression
unless true do
  1
else
  2
end

# AST
{:unless, [], [true, [do: 1, else: 2]]}
```

Now I've learnt about the AST and how to obtain it I can use it in the linter.
Say I want to forbid use of the `unless` macro with an `else` block as I think
it should be written with the `if` macro, to prevent the hard-to-read double
negative.


With an AST I can do this by walking the tree until I come across a node with
the atom `unless` in the first position, and then checking to see whether the
children includes an `else` block.

```elixir
defp check_unless({:unless, _, [_, [do: _, else: _]]}, status) do
  {node, :error}
end
defp check_unless(node, status) do
  {node, status}
end

Macro.prewalk(ast, :ok, &check_unless/2)
# {:ok, :error}
```

Traversing the AST is easy thanks to the `Macro.preawalk` function, which
takes an AST, an accumulator, and a callback that will receive each node. My
`check_unless/2` callback has two clauses. The first one pattern matches
against offending nodes and returns the atom `:error` in place of the
accumulator, and the other clause is a catch all for all other nodes.

And just like that I had a working linter. All that was left was to write more
rules and to do some plumbing to run them and present errors to the user. I
couldn't believe how easy Elixir had made this task for me. If you'd like to
see what came of this project it can be found on GitHub and Hex under the name
`dogma`.



------------------------------------------------------------------------------

                            More stuff to come...

------------------------------------------------------------------------------


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
