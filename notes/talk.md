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

Here I tokenize this line of code, and back I get a name token with a value of
"div", a whitespace token, a word token with a value of "I'm", a whitespace
token, and a name token with a value of "spartacus". Great.

```elixir
:my_tokenizer.string('a(href="/about")')
```
```elixir
{:ok, [
  name:   'a',
  "(":    '(',
  word:   'href',
  eq:     '=',
  string: '"/about"', # <-- Quotes
  ")":    ')',
], _}
```

At first this seemed enough, but when tokenizing another line I discovered a
problem. When I receive a string token the value is the string as written in
the source code, when I actually want the value of the string.

To resolve this I make use of the final part of a Leex module.

```erlang
{String} : {token, {string, strValue(TokenChars)}}.
% ...snip...

Erlang code.

strValue(S) ->
  tl(lists:droplast(S)).
```

Here the token tuple for the string token has been updated to call a
function called "strValue" on the TokenChars before inserting it into the
tuple.

The definition of this helper function goes in the "Erlang code" section. It
simply drops the trailing quote from the charlist with the droplast function,
and then takes the tail from that to remove the preceeding quote.

```elixir
:my_tokenizer.string('a(href="/about")')
```
```elixir
{:ok, [
  name:   'a',
  "(":    '(',
  word:   'href',
  eq:     '=',
  string: '/about', # <-- No quotes
  ")":    ')',
], _}
```

Now I get the value I want for string tokens. Later I'll probably also want to
add helper functions for parsing numbers, handling escaped characters in
strings, and so on.

And with that I have a working tokenizer, so I can move onto building a super
simple AST. In the same way that Erlang tokenization supplies a tool for
tokenization it also supplies a tool for parsing, the Yecc module. Like Leex
it's used by writing a module with a specific syntax and file extension, which
it then compiles into an Erlang module. This module contains a grammar, which
is a set of rules that describe the syntax of a language.


```erlang
%%% my_parser.yrl

Nonterminals
.

Terminals
.

Rootsymbol
.

%% Grammar rules here...

Erlang code.

%% Helper functions here...
```

The file consists of 5 main sections. Nonterminals, Terminals, Rootsymbol,
grammar rules, and another Erlang code section for helper functions.

Terminals are the the most basic symbols recognised by the grammar, they
cannot be broken down into smaller parts. In this case these are all the
token types my Leex tokenizer can create.


```erlang
%%% my_parser.yrl

Nonterminals
.

Terminals
'(' ')' name word dot hash string eq ws.

Rootsymbol
.

%% Grammar rules here...

Erlang code.

%% Helper functions here...
```

Nonterminals are higher level symbols that are formed by composing
nonterminals or other terminals.

The Rootsymbol is the highest level nonterminal symbol that composes all the
others.

Grammar rules are definitions about what symbols compose other symbols, and in
what context, and Erlang code is again where we keep helper functions.

Lets take a look at Nonterminals.

```pug
h1.jumbo
```

```elixir
nonterminal name: 'h1'
nonterminal dot:  '.'
nonterminal name: 'jumbo'
```

```elixir
nonterminal name:  'h1'
terminal    class: 'jumbo'
```

An example nonterminal in my grammar would be a class literal.

In my syntax this is a h1 element with the class of "jumbo".

It tokenizes to three nonterminal, the name "h1", a dot, and the name "jumbo".

In this context the "dot" nonterminal followed by the "name" nonterminal can be
composed together to form the "class" terminal.

```erlang
class -> dot name  % .btn
id    -> hash name % #jumbo
```

Lets quickly go through some nonterminals.

A class is a dot then a name.
An id is a hash then a name.

```erlang
classes -> class         % .big
classes -> class classes % .small.tiny.timid
```

Elements can have as many class literals on them as the user likes, so we need
a classes nonterminal.

classes are either a single class, or many classes. A repeating symbol is
defined recursively, so here classes can be a class followed by classes.

```erlang
names -> name            % div
names -> name id         % div#header
names -> name classes    % div.btn
names -> name id classes % div#submit.btn
names -> classes         % .grey.small
names -> id              % #jumbo
names -> id classes      % #jumbo.bordered
```

Names is the head of an element in the HTML shorthand syntax.

It can be a name
A name then an id
A name then classes
A name then an id then classes
Just classes
Just an id
Or an id then classes.

These declaritive rules continue. There's rules defining an attribute, many
attributes, pieces of text, and content that is composed of many pieces of
text and whitespace, until finally we reach the Rootsymbol, an element.

```erlang
element -> names                    % a.btn
element -> names attributes         % a.btn(href="/about")
element -> names attributes content % a.btn(href="/about") About
element -> names content            % a.btn About
```

An element can be just a set of names,
or it can be a set of names followed by attributes,
or it can be names, then attributes, then content,
or it can be just names followed by content.

And now that there are definitions for all the different symbols, from the
lowest terminals to the rootsymbol nonterminal Yecc has enough information to
parse an element from a set of tokens. The only thing left to do before it is
capable of generating an abstract syntax tree is instructing it how to build
a data structure for each nonterminal.

```erlang
class -> dot name % '$1' is the dot symbol
                  % '$2' is the name symbol
```

In my mini AST I want the class token to be represented by a string with the
same value as the token.

To achieve this I need to be able to refer to the tuple that makes up the name
token, and then extract the second value from it.

Helpfully Yecc assigns pseudo variables in the form of atoms for each symbol
used in the symbol definition. If class is a dot then a name, atom dollar one
refers to the dollar token, and atom dollar two refers to the name token.

```erlang
% .btn
'$1' = {dot,  "."}
'$2' = {name, "btn"}
```
```erlang
class -> dot name : element(2, '$2'). % "btn"
```

The string I want is the second element in the tuple, so I can call the
`element` function on dollar two to get it. This code is placed after a colon
and before a full stop for each definition.

```erlang
class -> dot  name : element(2, '$2').
id    -> hash name : element(2, '$2').

classes -> class         : ['$1'].
classes -> class classes : ['$1' | '$2'].
```

Now I've defined a data structure for class I can do the same for ID.

Some nodes in my AST will be collections represented with a list, one such
example is the "classes" symbol, which is one or many class symbols.

For the base case of just one class I wrap the class, which is a string as
defined above, in a list.

For the case of a class followed by classes we prepend the value of the class
to the value of classes, which unfolds recursively until we only have one
class, which is the list case we just defined.

Other symbols more complex than simple values or lists. The "names" symbol
consisted of a combination of a type value such as "div" or "body", an id, and
one or more classes. This could be represented as a three item tuple, but then
it's really hard to remember which field is which with tuples, so I'd like to
avoid having to.

```erlang
% records.hrl
-record(
  names,
  { type    = "div"
  , id      = nil
  , classes = []
  }
).
```

Since we're in Erlang land a record felt like the right tool for the job here.
If you're not familiar with Erlang records they're tuples with named fields,
and each field can be set and accessed by name, much easier than remembering
indexes.

Like Elixir structs each field in a record definition gets a default value.
I've got for the string "div" for the type, as the div element is a good HTML
element to fall back to, and some suitable bottom values for id and classes.


```erlang
names -> classes      : #names{ classes = '$1' }.
names -> name classes : #names{ classes = '$2', type = '$1' }.
names -> id   classes : #names{ classes = '$2', id   = '$1' }.
names -> id           : #names{ id = '$1' }.
names -> name         : #names{ type = element(2, '$1') }.
names -> name id      : #names{ type = element(2, '$1'), id = '$2' }.
names -> name id classes : #names{ type = element(2, '$1')
                                 , class = '$3' , id = '$2' }.
```

With this record I can use a nice named literal syntax for setting the various
values, and any that I neglect to fill in will use the default value specified
in the record definition.

With methods for building simple values, more complex values, and collections
of values I can work my way all the way up to the rootsymbol, the element.

```pug
a.profile(href="/me") User profile
```

```erlang
#element
{ type       = "a"
, id         = nil
, classes    = ["profile"]
, attributes = [{"href", "/me"}]
, content    = "User profile"
}
```

And with that I can turn a line of HTML shorthand into a data structure I can
use in Elixir! Now to turn this into a function.

This was another point at which I wasn't sure how to implement the next step.
I had a look on GitHub and discovered that other Elixir template libraries
would from here build EEx template strings and run these through the EEx
compiler. To me this felt like the wrong solution. EEx is written in Elixir,
how does it solve this problem?

I spent half an hour reading the EEx application in the Elixir lang repo, and
was pleasantly surprised by what I found there. The code was easy to
understand, and leveraged some of Elixir's metaprogramming features in a way
that to me seemed really clever and also easy to imitate. Yet again I felt
like Elixir was doing all the hard work for me.

```eex
Hello, <%= @name %>!
```
```elixir
tmpl = "Hello, <%= @name %>!"

EEx.eval_string(tmpl, assigns: [name: "world"])

# => "Hello, world!"
```

This what an EEx template looks like. It's a load of text in any format, with
these percent equals tags that contain Elixir code. The contents of the code
is evaluated when the function is called, and the return value is inserted
into the function.

```eex
Hello, <%= name %>!
```
```elixir
[ text: "Hello, ",
  expr: " name ",
  text: "!" ]
```

EEx's parser splits the template into text and expressions. This template
would be split into the text "Hello, ", an expression consisting of variable
name, and the text bang.

This needs to be turned into an expression which can be the body of a
function.

```elixir
def render(name) do
  "" <> "Hello, " <> name <> "!"
end
```

Here's a function that's equivilent to our template. It starts with an empty
string, and then for each value in the list of text and expressions it
concatenates it onto the value of the previous expression.

In an ideal world a function like this would be constructed by EEx from this
template. It also just so happens that this is more or less the function is
constructs though some clever metaprogramming.

As well as giving us `quote`, which turn expressions into abstract syntax
trees, Elixir also supplies `unquote`, which turns abstract syntax trees into
expressions at compile time. With `unquote` EEx can create this function by
manually reconstructing it's AST. Let's look at how this could be done.

```elixir
concat = fn
  ({:text, text}, buffer) ->
    quote do
      unquote(buffer) <> unquote(text)
    end

  ({:expr, text}, buffer) ->
    ast = Code.string_to_quoted!(text)
    quote do
      unquote(buffer) <> unquote(ast)
    end
end

compile = fn(list) ->
  Enum.reduce(list, "", concat)
end
```

At the bottom here is a compile function. It reduces over the list with a
concat function, and an empty string as the starting value.

The concat function has two clauses. The first is for text elements, which it
concatenates onto the buffer. Doing it inside the quote block like this
results in an AST being returned rather than the expression being evaluated.

The other clause is for expressions. It works exactly the same way as the
previous clause, except it calls the `Code.string_to_quoted!` function on the
value first in order to transform it from a string of Elixir code into an
AST to be injected into the quote block.

```elixir
compile.([text: "Hello ", expr: "name", text: "!"])


# {:<>, [context: Elixir, import: Kernel],
#  [{:<>, [context: Elixir, import: Kernel],
#    [{:<>, [context: Elixir, import: Kernel], ["", "Hello "]},
#     {:name, [line: 1], nil}]}, "!"]}
```
```elixir
compile.([text: "Hello ", expr: "name", text: "!"])
|> Macro.to_string
# (("" <> "Hello ") <> name) <> "!"
```

Here it is called on the list before.

As you can see it gets pretty hard to read these expressions pretty quickly,
so I'm converting back into a string with the `Macro.to_string` function.

```elixir
def render(name) do
  "" <> "Hello, " <> name <> "!"
end
```
```elixir
(("" <> "Hello ") <> name) <> "!"
```

It's pretty much exactly the same as the body function I wanted to generate.

So how would this work for the HTML templating library?

```pug
h1#title = name
```
```elixir
%Element{ type: "h1", id: "title", classes: [],
  attributes: [], content: "= name", }
```

The parser is used to generate an Elixir data structure with all the required
information from the template.

```pug
h1#title = name
```
```elixir
[ text: "<h1 id='title'>",
  expr: " name",
  text: "</h1>" ]
```

From the data structure a list of text and expressions is formed. Each element
consists of three parts, an opening tag, the contents, and a closing tag.


```elixir
defmodule View do
  @ast compile.([ text: "<h1 id='title'>", expr: " name", text: "</h1>" ])
  def render(name) do
    unquote(ast)
  end
end

view.render("Elixir")
# <h1 id='title'>Elixir</h1>
```

The parser built earlier builds
