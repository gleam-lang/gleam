Nonterminals
document literal tuple list elements element call module_def.

Terminals
'[' ']' '(' ')' ',' '.' identifier num atom string module.

Rootsymbol document.

document -> module_def    : ['$1'].
document -> literal       : ['$1'].
document -> list          : ['$1'].
document -> tuple         : ['$1'].
document -> call          : ['$1'].

module_def -> module identifier : {module, [l('$2')], v('$2')}.

call -> identifier tuple
        : {v('$1'), [l('$1')], tuple_to_list('$2')}.
call -> identifier '.' identifier tuple
        : {'.', [l('$1')], [v('$1'), v('$3')], tuple_to_list('$4')}.

list -> '[' ']'          : [].
list -> '[' elements ']' : '$2'.

tuple -> '(' ')'          : {}.
tuple -> '(' elements ')' : list_to_tuple('$2').

elements -> element              : ['$1'].
elements -> element ','          : ['$1'].
elements -> element ',' elements : ['$1'|'$3'].

element -> literal : '$1'.
element -> tuple   : '$1'.
element -> list    : '$1'.
element -> call    : '$1'.

literal -> num    : v('$1').
literal -> string : v('$1').
literal -> atom   : v('$1').

Erlang code.

v({_, _, V}) ->
  V.

l({_, L, _})->
  {line, L}.
