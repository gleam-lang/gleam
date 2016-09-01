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

Some time late I found myself making a simple web app in Elixir. Nothing
exciting, it rendered a few HTML pages to a user and let them record some
information in a database using forms. While I was writing the HTML views I
found myself getting a little fed up of Elixir default templating language for
making web pages. EEx is fast and easy to use, but with it I still have to
write regular HTML, and let's be honest, HTML is not fun. It has all these
superfluous angle brackets, a rather verbose syntax for closing tags, and you
have to manually escape certain characters. I would rather avoid doing all
this typing, and when working with Ruby and Javascript I can.

```pug
html
  head
    title Build Your Own Elixir
  body
    h1#conf An Elixir LDN talk
```

There's a templating system for Ruby called Slim and another for Javascript
called Pug which allow me to write HTML like this. All the superfluous syntax
is gone, and the delimeters have been replaced with indentation. Granted, this
isn't everyone's cup of tea, but I've become accustomed to it, and again I
found myself missing something in Elixir that I had elsewhere. Armed with my
new-found knowledge of tokenization and parsing I decided to make a similar
library for Elixir.

```
HTML Template -> (data -> HTML String)
```

A templating library is effectvely a function that takes a template of
alternative HTML syntax, and returns a function that given data produces a
string of HTML.

```
HTML Template -> Tokens -> AST -> (data -> HTML String)
```

In order to know what HTML to generate from the lightweight syntax I need to
yet again do some analysis on the source code, so like with the linter I need
to generate an AST to inspect.

Unlike with the linter I don't have a pre-built function for getting tokens
from my html templates, so I'll have to build my own. After a little digging I
discovered that the Erlang standard library includes a module called Leex
which offers a DSL for creating tokenizers. It's the module that the LFE
language uses for tokenization, which is a pretty good endorsement in my
books. It might be a little over the top as I could probably easily parse this
string by iterating over it, but this is a excuse to learn something new while
doing something useful, so lets get started.

```pug
html
  head
    title Build Your Own Elixir
  body
    h1(id="conf") Elixir LDN 2016!
```

One line is one element in my syntax, so I'll split on newlines and trim the
intendation, leaving me with just the element syntax that I want to parse.

```pug
h1#an-id
h2.class_a
h3.classB
h4(style="color: hotpink")
h5 Elixir LDN 2016!
```

Looking at these elements I can see a few token types.

There is one for names, which are element names, class names, or ID names,
such as "h1" or "classB" used here.

There are dots and hashes which are used to denote classes and IDs
respectively.

There are strings, which is a series of characters surrounded by double
quotes.

The syntax for attributes includes open paren tokens, close paren tokens,
and then an equals token between the attribute name and the value.

Lastly there's whitespace tokens, and word tokens, which are any
non-whitespace characters that are not covered by the other tokens.

Now I need to teach Leex what my tokens are so it can create the tokenizer.

```erlang
%%% my_tokenizer.xrl

Definitions.

% Token patterns here...

Rules.

% Mappings of patterns into token structures here...

Erlang code.

% Erlang helper functions here...
```

A Leex module is file that contains almost Erlang code and has the file
extension `.xrl`. Within it it has three sections: "Definitions" in which the
author uses reggular expressions to define each type of token, "Rules", in
which the author declares what data structure if any should result from each
token pattern being matched, and "Erlang code", which contains any helper
functions that might be used in the "Rules" section.

```erlang
Definitions.

Dot    = \.
Hash   = #
EQ     = =
OpenP  = \(
CloseP = \)
String = "([^\\""]|\\.)*"
Name   = [A-Za-z][A-Za-z0-9_-]*
WS     = [\s\t]+
Word   = [^\(\)\t\s\n\.#=]+
```

Here is my Leex "Definitions" section, containing all my various types of
tokens. Dot, Hash, EQ, OpenP and CloseP are all literals. Pattern names are
capitalized and go on the left hand side of the match operator, patterns go on
the right.

The string pattern is a pair of double quotes with zero or more characters
between them, where the characters are any non-double quote character, or any
character preceeded by an escaping slash.

A name is any letter, followed by any mix of letters, numbers, underscores and
dashs.

Whitespace is one or more spaces and tabs, and lastly a word is one or more or
anything else.

<!-- TODO: maybe remove? -->
The regex for "word" will match any text that also matches a name, it's less
specific. As a result whichever regex is checked with first will be the one
that matches, and because of this we need to control the order in which the
regexes are run. This isn't a problem with Leex, the definitions are checked
from top to bottom, and the first pattern that matches is used, much like a
case statement.

```erlang
Rules.

{String} : {token, {string, TokenChars}}.
{Name}   : {token, {name,   TokenChars}}.
{Word}   : {token, {word,   TokenChars}}.
{Hash}   : {token, {hash,   TokenChars}}.
{Dot}    : {token, {dot,    TokenChars}}.
{EQ}     : {token, {eq,     TokenChars}}.
{WS}     : {token, {ws,     TokenChars}}.
{OpenP}  : {token, {'(',    TokenChars}}.
{CloseP} : {token, {')',    TokenChars}}.

Erlang code.
```

After the "Definitions" section comes the "Rules" section, which is the
mapping between a token definition and a token data structure. The syntax for
a rule is the name of a definition in curly braces on the left, an instruction
tuple on the right, and a colon in the middle. Each rule ends with a full
stop, like in regular Erlang.

The first element in the tuple is the atom "token", which is an instruction to
output a token when this definition matches. The second item is the data
structure to be formed for this token. Here I'm always forming 2 item tuples
which the atom name of the token in the first position, and the matched
characters in the second position, which I access through the magic variable
"TokenChars".

```elixir
:my_tokenizer.string('div I\'m spartacus')
```
```elixir
{:ok, [
  name: 'div',
  ws:   ' ',
  word: 'I\'m',
  ws:   ' ',
  name: 'spartacus',
], _}
```

And with that I have a working tokenizer! If I place this in the `src`
directory Mix will compile this to an Erlang module which exposes a `string/1`
function that takes a charlist of code and returns a list of tokens. Because
I used tuples with an atom as the first element for my tokens I get back an
Elixir keyword list like so.


<!-- TODO: explain the need for helper functions -->

```erlang
{String} : {token, {string, strValue(TokenChars)}}.
```

```erlang
Erlang code.
```
