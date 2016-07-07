Nonterminals
document literal tuple list elements element call module_def
expression expressions assignment.

Terminals
'[' ']' '(' ')' ',' '.' '='
identifier num atom string module.

Rootsymbol document.

document -> expressions : '$1'.

expressions -> expression             : ['$1'].
expressions -> expression expressions : ['$1'|'$2'].

expression -> module_def : '$1'.
expression -> assignment : '$1'.
expression -> literal    : '$1'.
expression -> list       : '$1'.
expression -> tuple      : '$1'.
expression -> call       : '$1'.

assignment -> identifier '=' expression
              : {'=', m('$1'), [v('$1'), '$3']}.

module_def -> module identifier
              : {module, m('$2'), v('$2')}.

call -> identifier tuple
        : {v('$1'), m('$1'), tuple_to_list('$2')}.
call -> identifier '.' identifier tuple
        : {'.', m('$1'), [v('$1'), v('$3')], tuple_to_list('$4')}.

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

% Value from terminal
v({_, _, V}) ->
  V.

% Meta from terminal
m({_, L, _})->
  [{line, L}].
