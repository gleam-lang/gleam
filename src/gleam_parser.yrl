Nonterminals
source module functions function
exprs expr literal args elems.

Terminals
'(' ')' ',' '='
int float true false atom string
name upname
kw_module kw_let.

Rootsymbol source.

% Left 300 '+'.
% Left 300 '-'.
% Right 100 '<'.
% Nonassoc 200 '=='.

source -> module      : '$1'.
source -> exprs : '$1'.

module -> kw_module upname functions : mk_module('$2', '$3').

functions -> function           : ['$1'].
functions -> function functions : ['$1'|'$2'].

function -> kw_let name '(' args ')' '=' exprs : mk_function('$2', '$4', '$7').

exprs -> expr       : ['$1'].
exprs -> expr exprs : ['$1'|'$2'].

expr -> literal       : '$1'.
expr -> name          : mk_var('$1').
expr -> '(' ')'       : mk_tuple([]).
expr -> '(' elems ')' : mk_tuple('$2').

args -> name          : [mk_arg('$1')].
args -> name ','      : [mk_arg('$1')].
args -> name ',' args : [mk_arg('$1'), '$3'].

elems -> expr               : ['$1'].
elems -> expr ','           : ['$1'].
elems -> expr ',' elems     : ['$1' | '$3'].

literal -> atom   : mk_literal('$1').
literal -> int    : mk_literal('$1').
literal -> float  : mk_literal('$1').
literal -> true   : mk_literal('$1').
literal -> false  : mk_literal('$1').
literal -> string : mk_literal('$1').

Erlang code.

-include("gleam_records.hrl").

mk_module({upname, _, Name}, Functions) ->
  #gleam_ast_module{name = Name, functions = Functions}.

mk_function({name, _, Name}, Args, Body) ->
  #gleam_ast_function{name = Name, args = Args, body = Body}.

mk_arg({name, _Line, Name}) -> Name.

mk_var({name, Line, Name}) -> #gleam_ast_var{line = Line, name = Name}.

mk_tuple(Elems) -> #gleam_ast_tuple{elems = Elems}.

mk_literal({atom, Line, Value})   -> #gleam_ast_atom{line = Line, value = Value};
mk_literal({int, Line, Value})    -> #gleam_ast_int{line = Line, value = Value};
mk_literal({float, Line, Value})  -> #gleam_ast_float{line = Line, value = Value};
mk_literal({true, Line, Value})   -> #gleam_ast_bool{line = Line, value = Value};
mk_literal({false, Line, Value})  -> #gleam_ast_bool{line = Line, value = Value};
mk_literal({string, Line, Value}) -> #gleam_ast_string{line = Line, value = Value}.
