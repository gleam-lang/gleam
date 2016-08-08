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
AST of any code we pass to it. What's more the Elixir standard library
contains an Erlang module called `:elixir_tokenizer`, which exposes a function
that will give us a way to get the tokens from a source code string, after
this it's just a matter of pattern matching on these forms to detect errors.
Simple

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
