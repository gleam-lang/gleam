Nonterminals
document literal tuple list elements call module_def
expression statement statements assignment variable.

Terminals
'[' ']' '(' ')' ',' '.' '='
identifier num atom string module.

Rootsymbol document.

Expect 1.

document -> statements : '$1'.

statements -> statement            : ['$1'].
statements -> statement statements : ['$1'|'$2'].

statement -> module_def : '$1'.
statement -> expression : '$1'.

assignment -> identifier '=' expression
              : {'=', m('$1'), [v('$1'), '$3']}.

module_def -> module identifier
              : {module, m('$2'), v('$2')}.

call -> identifier tuple
        : {v('$1'), m('$1'), tuple_to_list('$2')}.
call -> identifier '.' identifier tuple
        : {'.', m('$1'), [v('$1'), v('$3')], tuple_to_list('$4')}.

variable -> identifier
            : {variable, m('$1'), v('$1')}.

list -> '[' ']'          : [].
list -> '[' elements ']' : '$2'.

tuple -> '(' ')'          : {}.
tuple -> '(' elements ')' : list_to_tuple('$2').

elements -> expression              : ['$1'].
elements -> expression ','          : ['$1'].
elements -> expression ',' elements : ['$1'|'$3'].

expression -> assignment : '$1'.
expression -> variable   : '$1'.
expression -> literal    : '$1'.
expression -> tuple      : '$1'.
expression -> list       : '$1'.
expression -> call       : '$1'.

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
