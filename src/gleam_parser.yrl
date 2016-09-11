Nonterminals
literal tuple list elements call module_declaration
expression expressions statement statements assignment variable
function fn_block fn_statements fn_clause.

Terminals
'[' ']' '(' ')' '{' '}' ',' '.' '='
public private def
identifier num atom string module.

Rootsymbol statements.

Expect 1.

statements -> statement            : ['$1'].
statements -> statement statements : ['$1'|'$2'].

statement -> module_declaration : '$1'.
statement -> expression         : '$1'.
statement -> function           : '$1'.

assignment -> identifier '=' expression
            : {'=', m('$1'), [v('$1'), '$3']}.

module_declaration -> module identifier : {module, m('$2'), v('$2')}.

call -> identifier tuple
      : {v('$1'), m('$1'), tuple_to_list('$2')}.
call -> identifier '.' identifier tuple
      : {'.', m('$1'), [v('$1'), v('$3')], tuple_to_list('$4')}.

variable -> identifier : {variable, m('$1'), v('$1')}.

function -> public identifier fn_block
          : {function, m('$1'), public, v('$2'), '$3'}.
function -> private identifier fn_block
          : {function, m('$1'), private, v('$2'), '$3'}.

fn_block -> '{' fn_statements '}' : '$2'.

fn_statements -> fn_clause               : ['$1'].
fn_statements -> fn_clause fn_statements : ['$1'|'$2'].

fn_clause -> def tuple '{' expressions '}'
           : {def, m('$1'), tuple_to_list('$2'), '$4'}.

list -> '[' ']'          : [].
list -> '[' elements ']' : '$2'.

tuple -> '(' ')'          : {}.
tuple -> '(' elements ')' : list_to_tuple('$2').

elements -> expression              : ['$1'].
elements -> expression ','          : ['$1'].
elements -> expression ',' elements : ['$1'|'$3'].

expressions -> expression             : ['$1'].
expressions -> expression expressions : ['$1'|'$2'].

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
v({_, _, V}) -> V.

% Meta from terminal
m({_, L, _}) -> [{line, L}];
m({_, L})    -> [{line, L}].
