Nonterminals
document literal.

Terminals
num atom string.

Rootsymbol document.

document -> literal : ['$1'].

literal -> num    : v('$1').
literal -> string : v('$1').
literal -> atom   : v('$1').

Erlang code.

v({_, _, V}) ->
  V.
