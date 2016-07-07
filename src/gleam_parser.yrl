Nonterminals
document literal tuple list elements element.

Terminals
'[' ']' '(' ')' ',' num atom string.

Rootsymbol document.

document -> literal : ['$1'].
document -> list    : ['$1'].
document -> tuple   : ['$1'].

list -> '[' ']'              : [].
list -> '[' elements ']'     : '$2'.

tuple -> '(' ')'              : {}.
tuple -> '(' elements ')'     : list_to_tuple('$2').

elements -> element              : ['$1'].
elements -> element ','          : ['$1'].
elements -> element ',' elements : ['$1'|'$3'].

element -> literal : '$1'.

literal -> num    : v('$1').
literal -> string : v('$1').
literal -> atom   : v('$1').

Erlang code.

v({_, _, V}) ->
  V.
