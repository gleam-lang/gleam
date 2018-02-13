Nonterminals
source module functions function
exprs expr adt literal args elems
exports export export_names.

Terminals
'(' ')' '[' ']' '::'
',' '='
'<=' '<' '>' '>='
'/' '*' '+' '-' '/.' '*.' '+.' '-.'
int float atom string
name upname call
kw_module kw_fn kw_export.

Rootsymbol source.

Nonassoc 300 '+'.
Nonassoc 300 '-'.
Nonassoc 300 '+.'.
Nonassoc 300 '-.'.
Right 60 '::'.
Left 220 '*'.
Left 220 '/'.
Left 220 '*.'.
Left 220 '/.'.
Left 160 '<'.
Left 160 '>'.
Left 160 '<='.
Left 160 '>='.
% Left 170 '|>'.

source -> module : '$1'.
source -> exprs  : '$1'.

module -> kw_module upname                   : module('$2', [], []).
module -> kw_module upname functions         : module('$2', [], '$3').
module -> kw_module upname exports           : module('$2', '$3', []).
module -> kw_module upname exports functions : module('$2', '$3', '$4').

exports -> export         : '$1'.
exports -> export exports : '$1' ++ '$2'.

export -> kw_export export_names : '$2'.

export_names -> name '/' int                  : [export('$1', '$3')].
export_names -> name '/' int ',' export_names : [export('$1', '$3') | '$5'].

functions -> function           : ['$1'].
functions -> function functions : ['$1'|'$2'].

function -> kw_fn call ')' '=' exprs      : function('$2', [], '$5').
function -> kw_fn call args ')' '=' exprs : function('$2', '$3', '$6').

exprs -> name '=' expr exprs : [assignment('$1', '$3', '$4')].
exprs -> expr                : ['$1'].
exprs -> expr exprs          : ['$1'|'$2'].

expr -> literal        : '$1'.
expr -> adt            : '$1'.
expr -> name           : var('$1').
expr -> call elems ')' : local_call('$1', '$2').
expr -> expr '::' expr : local_call('::', ['$1', '$3']).
expr -> expr '+' expr  : local_call('+', ['$1', '$3']).
expr -> expr '-' expr  : local_call('-', ['$1', '$3']).
expr -> expr '*' expr  : local_call('*', ['$1', '$3']).
expr -> expr '/' expr  : local_call('/', ['$1', '$3']).
expr -> expr '+.' expr : local_call('+.', ['$1', '$3']).
expr -> expr '-.' expr : local_call('-.', ['$1', '$3']).
expr -> expr '*.' expr : local_call('*.', ['$1', '$3']).
expr -> expr '/.' expr : local_call('/.', ['$1', '$3']).
expr -> expr '<=' expr : local_call('<=', ['$1', '$3']).
expr -> expr '<'  expr : local_call('<' , ['$1', '$3']).
expr -> expr '>'  expr : local_call('>' , ['$1', '$3']).
expr -> expr '>=' expr : local_call('>=', ['$1', '$3']).

args -> name          : [arg('$1')].
args -> name ','      : [arg('$1')].
args -> name ',' args : [arg('$1') | '$3'].

elems -> expr           : ['$1'].
elems -> expr ','       : ['$1'].
elems -> expr ',' elems : ['$1' | '$3'].

adt -> upname : adt('$1', []).

literal -> '(' ')'        : tuple([]).
literal -> '(' elems ')'  : tuple('$2').
literal -> '[' ']'        : list([]).
literal -> '[' elems ']'  : list('$2').
literal -> atom           : literal('$1').
literal -> int            : literal('$1').
literal -> float          : literal('$1').
literal -> string         : literal('$1').

Erlang code.

-include("gleam_records.hrl").

module({upname, _, Name}, Exports, Functions) ->
  #gleam_ast_module{name = Name, exports = Exports, functions = Functions}.

local_call({call, _, Name}, Args) ->
  #gleam_ast_local_call{name = Name, args = Args};
local_call(Name, Args) when is_atom(Name) ->
  #gleam_ast_local_call{name = Name, args = Args}.

% call(Mod, Name, Args) ->
%   #gleam_ast_call{module = Mod, name = Name, args = Args}.

function({call, _, Name}, Args, Body) ->
  #gleam_ast_function{name = Name, args = Args, body = Body}.

assignment({name, _, Name}, Value, Then) ->
  #gleam_ast_assignment{name = Name, value = Value, then = Then}.

arg({name, _Line, Name}) -> Name.

var({name, Line, Name}) -> #gleam_ast_var{line = Line, name = Name}.

export({name, _, Name}, {int, _, Arity}) -> {Name, Arity}.

tuple(Elems) -> #gleam_ast_tuple{elems = Elems}.

list(Elems) -> #gleam_ast_list{elems = Elems}.

adt({upname, Line, Name}, Elems) ->
  #gleam_ast_adt{name = Name, line = Line, elems = Elems}.

literal({atom, Line, Value})   -> #gleam_ast_atom{line = Line, value = Value};
literal({int, Line, Value})    -> #gleam_ast_int{line = Line, value = Value};
literal({float, Line, Value})  -> #gleam_ast_float{line = Line, value = Value};
literal({string, Line, Value}) -> #gleam_ast_string{line = Line, value = Value}.
