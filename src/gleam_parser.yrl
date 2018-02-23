Nonterminals
source module functions function
exprs expr binary_call literal elems args call_args
container container_pattern elems_pattern
exports export export_names pattern
case_expr case_clauses case_clause field fields.

Terminals
'(' ')' '[' ']' '::' '{' '}'
',' '=' '|' '=>' '|>'
'<=' '<' '>' '>='
'.'
'/' '*' '+' '-' '/.' '*.' '+.' '-.'
int float atom string
hole name upname call upcall
kw_module kw_fn kw_export kw_case.

Rootsymbol source.

Left 160 '<'.
Left 160 '<='.
Left 160 '>'.
Left 160 '>='.
Left 180 '|>'.
Left 210 '+'.
Left 210 '+.'.
Left 210 '-'.
Left 210 '-.'.
Left 220 '*'.
Left 220 '*.'.
Left 220 '/'.
Left 220 '/.'.
Right 60 '::'.
Right 70 '|'.

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

exprs -> name '=' expr exprs : [assignment('$2', '$1', '$3', '$4')].
exprs -> expr                : ['$1'].
exprs -> expr exprs          : ['$1'|'$2'].

expr -> literal                    : '$1'.
expr -> container                  : '$1'.
expr -> case_expr                  : '$1'.
expr -> binary_call                : '$1'.
expr -> name                       : var('$1').
expr -> expr '.' name              : record_access('$2', '$1', '$3').
expr -> call ')'                   : local_call('$1', []).
expr -> call call_args ')'         : local_call('$1', '$2').
expr -> expr '|>' expr             : pipe('$2', '$1', '$3').
expr -> expr '.' '(' ')'           : closure_call('$2', '$1', []).
expr -> expr '.' '(' call_args ')' : closure_call('$2', '$1', '$3').

binary_call -> expr '::' expr : cons('$2', '$1', '$3').
binary_call -> expr '+' expr  : local_call('$2', ['$1', '$3']).
binary_call -> expr '-' expr  : local_call('$2', ['$1', '$3']).
binary_call -> expr '*' expr  : local_call('$2', ['$1', '$3']).
binary_call -> expr '/' expr  : local_call('$2', ['$1', '$3']).
binary_call -> expr '+.' expr : local_call('$2', ['$1', '$3']).
binary_call -> expr '-.' expr : local_call('$2', ['$1', '$3']).
binary_call -> expr '*.' expr : local_call('$2', ['$1', '$3']).
binary_call -> expr '/.' expr : local_call('$2', ['$1', '$3']).
binary_call -> expr '<=' expr : local_call('$2', ['$1', '$3']).
binary_call -> expr '<'  expr : local_call('$2', ['$1', '$3']).
binary_call -> expr '>'  expr : local_call('$2', ['$1', '$3']).
binary_call -> expr '>=' expr : local_call('$2', ['$1', '$3']).

case_expr -> kw_case expr case_clauses : case_expr('$1', '$2', '$3').

case_clauses -> case_clause              : ['$1'].
case_clauses -> case_clause case_clauses : ['$1'|'$2'].

case_clause -> '|' pattern '=>' expr : case_clause('$1', '$2', '$4').

call_args -> hole               : [hole()].
call_args -> hole ','           : [hole()].
call_args -> hole ',' call_args : [hole() | '$3'].
call_args -> expr               : ['$1'].
call_args -> expr ','           : ['$1'].
call_args -> expr ',' call_args : ['$1' | '$3'].

args -> name          : [arg('$1')].
args -> name ','      : [arg('$1')].
args -> name ',' args : [arg('$1') | '$3'].

container -> upname           : adt('$1', []).
container -> upcall elems ')' : adt('$1', '$2').
container -> '(' ')'          : tuple('$1', []).
container -> '(' elems ')'    : tuple('$1', '$2').
container -> '[' ']'          : list('$1', []).
container -> '[' elems ']'    : list('$1', '$2').
container -> '{' '}'          : record('$1', []).
container -> '{' fields '}'   : record('$1', '$2').

elems -> expr           : ['$1'].
elems -> expr ','       : ['$1'].
elems -> expr ',' elems : ['$1' | '$3'].

fields -> field             : ['$1'].
fields -> field ','         : ['$1'].
fields -> field ',' fields  : ['$1' | '$3'].

field -> name '=' expr      : record_field('$1', '$3').

pattern -> literal              : '$1'.
pattern -> container_pattern    : '$1'.
pattern -> name                 : var('$1').
pattern -> hole                 : hole().
pattern -> pattern '::' pattern : cons('$2', '$1', '$3').

container_pattern -> upname                   : adt('$1', []).
container_pattern -> upcall elems_pattern ')' : adt('$1', '$2').
container_pattern -> '(' ')'                  : tuple('$1', []).
container_pattern -> '(' elems_pattern ')'    : tuple('$1', '$2').
container_pattern -> '[' ']'                  : list('$1', []).
container_pattern -> '[' elems_pattern ']'    : list('$1', '$2').

elems_pattern -> pattern                   : ['$1'].
elems_pattern -> pattern ','               : ['$1'].
elems_pattern -> pattern ',' elems_pattern : ['$1' | '$3'].

literal -> '|' '|' expr      : closure('$1', [], '$3').
literal -> '|' args '|' expr : closure('$1', '$2', '$4').
literal -> atom              : literal('$1').
literal -> int               : literal('$1').
literal -> float             : literal('$1').
literal -> string            : literal('$1').

Erlang code.

-include("gleam_records.hrl").

module({upname, _, Name}, Exports, Functions) ->
  #ast_module{name = Name, exports = Exports, functions = Functions}.

closure({'|', Meta}, Args, Body) ->
  #ast_closure{meta = Meta, args = Args, body = Body}.

cons({'::', Meta}, Head, Tail) ->
  #ast_cons{meta = Meta, head = Head, tail = Tail}.

local_call({Operator, Meta}, Args) ->
  #ast_local_call{meta = Meta, name = Operator, args = Args};
local_call({call, Meta, Name}, Args) ->
  #ast_local_call{meta = Meta, name = Name, args = Args}.

closure_call({'.', Meta}, Closure, Args) ->
  #ast_closure_call{meta = Meta, closure = Closure, args = Args}.

function({call, Meta, Name}, Args, Body) ->
  #ast_function{meta = Meta, name = Name, args = Args, body = Body}.

assignment({'=', Meta}, {name, _, Name}, Value, Then) ->
  #ast_assignment{meta = Meta, name = Name, value = Value, then = Then}.

arg({name, _Meta, Name}) ->
  Name.

var({name, Meta, Name}) ->
  #ast_var{meta = Meta, name = Name}.

export({name, _, Name}, {int, _, Arity}) ->
  {Name, Arity}.

record({'{', Meta}, Fields) ->
  #ast_record{meta = Meta, fields = Fields}.

record_field({name, Meta, Key}, Value) ->
  #ast_record_field{meta = Meta, key = Key, value = Value}.

record_access({'.', Meta}, Record, {name, _, Key}) ->
  #ast_record_access{meta = Meta, record = Record, key = Key}.

tuple({'(', Meta}, Elems) ->
  #ast_tuple{meta = Meta, elems = Elems}.

list({'[', Meta}, Elems) ->
  #ast_list{meta = Meta, elems = Elems}.

adt({Type, Meta, Name}, Elems) when Type =:= upname; Type =:= upcall ->
  #ast_adt{name = Name, meta = Meta, elems = Elems}.

case_expr({kw_case, Meta}, Subject, Clauses) ->
  #ast_case{meta = Meta, subject = Subject, clauses = Clauses}.

case_clause({'|', Meta}, Pattern, Value) ->
  #ast_clause{meta = Meta, pattern = Pattern, value = Value}.

pipe({'|>', Meta}, Lhs, Rhs) ->
  #ast_pipe{meta = Meta, lhs = Lhs, rhs = Rhs}.

hole() ->
  hole.

literal({atom, Meta, Value}) ->
  #ast_atom{meta = Meta, value = Value};
literal({int, Meta, Value}) ->
  #ast_int{meta = Meta, value = Value};
literal({float, Meta, Value}) ->
  #ast_float{meta = Meta, value = Value};
literal({string, Meta, Value}) ->
  #ast_string{meta = Meta, value = Value}.
