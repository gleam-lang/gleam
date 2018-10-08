Nonterminals
source module
function test
exprs expr operator literal elems args call_args
container container_pattern elems_pattern
pattern
case_expr case_clauses case_clause field fields.

Terminals
'(' ')' '[' ']' '::' '{' '}'
',' '=' '|' '=>' '|>'
'<=' '<' '>' '>=' '==' '!='
'.'
'/' '*' '+' '-' '/.' '*.' '+.' '-.'
int float atom string
hole name upname upcall
kw_fn kw_fn_call kw_case kw_test
kw_raise kw_throw kw_pub.

Rootsymbol source.

Left 150 '!='.
Left 150 '=='.
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
Left 230 '.'.
Left 300 '('.
Right 60 '::'.
Right 70 '|'.

source -> module : '$1'.
source -> exprs  : '$1'.

module -> kw_pub function        : mod_fun(true, '$2', #ast_module{}).
module -> kw_pub function module : mod_fun(true, '$2', '$3').
module -> function               : mod_fun(false, '$1', #ast_module{}).
module -> function module        : mod_fun(false, '$1', '$2').
module -> test                   : mod_test('$1', #ast_module{}).
module -> test module            : mod_test('$1', '$2').

function -> kw_fn name '(' ')' '{' exprs '}'      : function('$2', [], '$6').
function -> kw_fn name '(' args ')' '{' exprs '}' : function('$2', '$4', '$7').

test -> kw_test name '{' exprs '}' : test('$2', '$4').

exprs -> name '=' expr exprs : assignment('$2', '$1', '$3', '$4').
exprs -> expr                : '$1'.
exprs -> expr exprs          : seq('$1', '$2').

expr -> literal                    : '$1'.
expr -> container                  : '$1'.
expr -> case_expr                  : '$1'.
expr -> operator                   : '$1'.
expr -> name                       : var('$1').
expr -> expr '.' name              : record_select('$2', '$1', '$3').
expr -> expr '(' ')'               : local_call('$2', '$1', []).
expr -> expr '(' call_args ')'     : local_call('$2', '$1', '$3').
expr -> expr '.' '(' ')'           : fn_call('$2', '$1', []).
expr -> expr '.' '(' call_args ')' : fn_call('$2', '$1', '$4').
expr -> kw_raise expr ')'          : raise('$1', '$2').
expr -> kw_throw expr ')'          : throw_('$1', '$2').

operator -> expr '|>' expr : operator('$2', ['$1', '$3']).
operator -> expr '::' expr : cons('$2', '$1', '$3').
operator -> expr '+' expr  : operator('$2', ['$1', '$3']).
operator -> expr '-' expr  : operator('$2', ['$1', '$3']).
operator -> expr '*' expr  : operator('$2', ['$1', '$3']).
operator -> expr '/' expr  : operator('$2', ['$1', '$3']).
operator -> expr '+.' expr : operator('$2', ['$1', '$3']).
operator -> expr '-.' expr : operator('$2', ['$1', '$3']).
operator -> expr '*.' expr : operator('$2', ['$1', '$3']).
operator -> expr '/.' expr : operator('$2', ['$1', '$3']).
operator -> expr '<=' expr : operator('$2', ['$1', '$3']).
operator -> expr '<'  expr : operator('$2', ['$1', '$3']).
operator -> expr '>'  expr : operator('$2', ['$1', '$3']).
operator -> expr '>=' expr : operator('$2', ['$1', '$3']).
operator -> expr '==' expr : operator('$2', ['$1', '$3']).
operator -> expr '!=' expr : operator('$2', ['$1', '$3']).

case_expr -> kw_case expr '{' case_clauses '}' : case_expr('$1', '$2', '$4').

case_clauses -> case_clause              : ['$1'].
case_clauses -> case_clause case_clauses : ['$1'|'$2'].

case_clause -> '|' pattern '=>' expr : case_clause('$1', '$2', '$4').

call_args -> hole               : [hole('$1')].
call_args -> hole ','           : [hole('$1')].
call_args -> hole ',' call_args : [hole('$1') | '$3'].
call_args -> expr               : ['$1'].
call_args -> expr ','           : ['$1'].
call_args -> expr ',' call_args : ['$1' | '$3'].

args -> name          : [arg('$1')].
args -> name ','      : [arg('$1')].
args -> name ',' args : [arg('$1') | '$3'].

container -> upname           : adt('$1', []).
container -> upcall elems ')' : adt('$1', '$2').
container -> '{' elems '}'    : tuple('$1', '$2').
container -> '[' ']'          : list('$2', []).
container -> '[' elems ']'    : list('$3', '$2').
container -> '{' '}'          : #ast_record_empty{}.
container -> '{' fields '}'   : record('$1', '$2').

elems -> expr           : ['$1'].
elems -> expr ','       : ['$1'].
elems -> expr ',' elems : ['$1' | '$3'].

fields -> field             : ['$1'].
fields -> field ','         : ['$1'].
fields -> field ',' fields  : ['$1' | '$3'].

field -> name '=>' expr      : record_field('$1', '$3').

pattern -> literal              : '$1'.
pattern -> container_pattern    : '$1'.
pattern -> name                 : var('$1').
pattern -> hole                 : hole('$1').
pattern -> pattern '::' pattern : cons('$2', '$1', '$3').

container_pattern -> upname                   : adt('$1', []).
container_pattern -> upcall elems_pattern ')' : adt('$1', '$2').
container_pattern -> '(' ')'                  : tuple('$1', []).
container_pattern -> '(' elems_pattern ')'    : tuple('$1', '$2').
container_pattern -> '{' elems_pattern '}'    : tuple('$1', '$2').
container_pattern -> '[' ']'                  : list('$2', []).
container_pattern -> '[' elems_pattern ']'    : list('$3', '$2').

elems_pattern -> pattern                   : ['$1'].
elems_pattern -> pattern ','               : ['$1'].
elems_pattern -> pattern ',' elems_pattern : ['$1' | '$3'].

literal -> kw_fn_call ')' '{' exprs '}'      : fn('$1', [], '$4').
literal -> kw_fn_call args ')' '{' exprs '}' : fn('$1', '$2', '$5').
literal -> atom              : literal('$1').
literal -> int               : literal('$1').
literal -> float             : literal('$1').
literal -> string            : literal('$1').

Erlang code.

-include("gleam_records.hrl").

mod_fun(true, Function, Module) ->
  #ast_module{statements = Statements, exports = Exports} = Module,
  #ast_mod_fn{name = Name, args = Args} = Function,
  Module#ast_module{statements = [Function | Statements],
                    exports = [{Name, length(Args)} | Exports]};
mod_fun(false, Function, Module) ->
  #ast_module{statements = Statements} = Module,
  Module#ast_module{statements = [Function | Statements]}.

mod_test(Test, Module) ->
  #ast_module{statements = Statements} = Module,
  Module#ast_module{statements = [Test | Statements]}.

seq(First, Then) ->
  #ast_seq{first = First, then = Then}.

raise({kw_raise, Meta}, Value) ->
  #ast_raise{meta = Meta, value = Value}.

throw_({kw_throw, Meta}, Value) ->
  #ast_throw{meta = Meta, value = Value}.

test({name, Meta, Name}, Body) ->
  #ast_mod_test{meta = Meta, name = Name, body = Body}.

fn({_, Meta}, Args, Body) ->
  #ast_fn{meta = Meta, args = Args, body = Body}.

operator({Operator, Meta}, Args) ->
  #ast_operator{meta = Meta, name = atom_to_list(Operator), args = Args}.

local_call({'(', Meta}, Fn, Args) ->
  #ast_local_call{meta = Meta, fn = Fn, args = Args}.

fn_call({'.', Meta}, Fn, Args) ->
  #ast_fn_call{meta = Meta, fn = Fn, args = Args}.

function({name, Meta, Name}, Args, Body) ->
  #ast_mod_fn{meta = Meta, name = Name, args = Args, body = Body}.

assignment({'=', Meta}, {name, _, Name}, Value, Then) ->
  #ast_assignment{meta = Meta, name = Name, value = Value, then = Then}.

arg({name, _Meta, Name}) ->
  Name.

var({name, Meta, Name}) ->
  #ast_var{meta = Meta, name = Name}.

record({'{', EmptyMeta}, Fields) ->
  Extend = fun({Label, Value}, Parent) ->
    Meta = #meta{} = element(2, Value),
    #ast_record_extend{meta = Meta,
                       label = Label,
                       value = Value,
                       parent = Parent}
  end,
  lists:foldl(Extend,
              #ast_record_empty{meta = EmptyMeta},
              lists:reverse(Fields)).

record_field({name, _Meta, Key}, Value) ->
  {Key, Value}.

record_select({'.', Meta}, Record, {name, _, Label}) ->
  #ast_record_select{meta = Meta, record = Record, label = Label}.

tuple({_, Meta}, Elems) ->
  #ast_tuple{meta = Meta, elems = Elems}.

cons({'::', Meta}, Head, Tail) ->
  #ast_cons{meta = Meta, head = Head, tail = Tail}.

list({']', NilMeta}, Elems) ->
  Cons = fun(Head, Tail) ->
    Meta = #meta{} = element(2, Head),
    #ast_cons{meta = Meta, head = Head, tail = Tail}
  end,
  lists:foldl(Cons, #ast_nil{meta = NilMeta}, lists:reverse(Elems)).

adt({Type, Meta, Name}, Elems) when Type =:= upname; Type =:= upcall ->
  #ast_adt{name = Name, meta = Meta, elems = Elems}.

case_expr({kw_case, Meta}, Subject, Clauses) ->
  #ast_case{meta = Meta, subject = Subject, clauses = Clauses}.

case_clause({'|', Meta}, Pattern, Value) ->
  #ast_clause{meta = Meta, pattern = Pattern, value = Value}.

hole({hole, Meta}) ->
  #ast_hole{meta = Meta}.

literal({atom, Meta, Value}) ->
  #ast_atom{meta = Meta, value = Value};
literal({int, Meta, Value}) ->
  #ast_int{meta = Meta, value = Value};
literal({float, Meta, Value}) ->
  #ast_float{meta = Meta, value = Value};
literal({string, Meta, Value}) ->
  #ast_string{meta = Meta, value = Value}.
