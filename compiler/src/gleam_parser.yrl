Nonterminals
source module statements statement
function test enum enum_defs enum_def
exprs expr literal elems args call_args
elems_pattern pattern
type type_args
case_clauses case_clause field fields.

Terminals
'(' ')' '[' ']' '::' '{' '}'
',' '=' '|' '|>' '->'
'<=' '<' '>' '>=' '==' '!='
'.'
'/' '*' '+' '-' '/.' '*.' '+.' '-.'
int float atom string
hole name upname
kw_fn kw_case kw_test
kw_raise kw_throw kw_pub kw_enum.

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

module -> statements : #ast_module{statements = '$1'}.

statements -> statement             : ['$1'].
statements -> statement statements  : ['$1' | '$2'].

statement -> function : '$1'.
statement -> test     : '$1'.
statement -> enum     : '$1'.

enum -> kw_pub kw_enum upname              '=' enum_defs : enum(true, '$3', [], '$5').
enum -> kw_pub kw_enum upname '(' args ')' '=' enum_defs : enum(true, '$3', '$5', '$8').
enum ->        kw_enum upname              '=' enum_defs : enum(false, '$2', [], '$4').
enum ->        kw_enum upname '(' args ')' '=' enum_defs : enum(false, '$2', '$4', '$7').

enum_defs -> enum_def           : ['$1'].
enum_defs -> enum_def enum_defs : ['$1' | '$2'].

enum_def -> '|' upname                   : enum_def('$2', []).
enum_def -> '|' upname '(' type_args ')' : enum_def('$2', '$4').

type_args -> type               : ['$1'].
type_args -> type ',' type_args : ['$1' | '$3'].

type -> upname : type_constructor('$1', []).
type -> name   : type_var('$1').

function -> kw_pub kw_fn name '('      ')' '{' exprs '}' : function(true, '$3', [], '$7').
function -> kw_pub kw_fn name '(' args ')' '{' exprs '}' : function(true, '$3', '$5', '$8').
function ->        kw_fn name '('      ')' '{' exprs '}' : function(false, '$2', [], '$6').
function ->        kw_fn name '(' args ')' '{' exprs '}' : function(false, '$2', '$4', '$7').

test -> kw_test name '{' exprs '}' : test('$2', '$4').

exprs -> name '=' expr exprs : assignment('$2', '$1', '$3', '$4').
exprs -> expr                : '$1'.
exprs -> expr exprs          : seq('$1', '$2').

expr -> literal                    : '$1'.
expr -> upname                     : enum_constructor('$1', []).
expr -> upname '(' elems ')'       : enum_constructor('$1', '$3').
expr -> '{' elems '}'              : tuple('$1', '$2').
expr -> '[' ']'                    : list('$2', []).
expr -> '[' elems ']'              : list('$3', '$2').
expr -> '{' '}'                    : #ast_record_empty{}.
expr -> '{' fields '}'             : record('$1', '$2').
expr -> '{' expr '|' fields '}'    : record_extend('$2', '$4').
expr -> name                       : var('$1').
expr -> expr '.' name              : record_select('$2', '$1', '$3').
expr -> expr '(' ')'               : local_call('$2', '$1', []).
expr -> expr '(' call_args ')'     : local_call('$2', '$1', '$3').
expr -> expr '.' '(' ')'           : fn_call('$2', '$1', []).
expr -> expr '.' '(' call_args ')' : fn_call('$2', '$1', '$4').
expr -> kw_raise expr ')'          : raise('$1', '$2').
expr -> kw_throw expr ')'          : throw_('$1', '$2').
expr -> expr '::' expr             : cons('$2', '$1', '$3').
expr -> expr '|>' expr             : op('$2', ['$1', '$3']).
expr -> expr '+' expr              : op('$2', ['$1', '$3']).
expr -> expr '-' expr              : op('$2', ['$1', '$3']).
expr -> expr '*' expr              : op('$2', ['$1', '$3']).
expr -> expr '/' expr              : op('$2', ['$1', '$3']).
expr -> expr '+.' expr             : op('$2', ['$1', '$3']).
expr -> expr '-.' expr             : op('$2', ['$1', '$3']).
expr -> expr '*.' expr             : op('$2', ['$1', '$3']).
expr -> expr '/.' expr             : op('$2', ['$1', '$3']).
expr -> expr '<=' expr             : op('$2', ['$1', '$3']).
expr -> expr '<'  expr             : op('$2', ['$1', '$3']).
expr -> expr '>'  expr             : op('$2', ['$1', '$3']).
expr -> expr '>=' expr             : op('$2', ['$1', '$3']).
expr -> expr '==' expr             : op('$2', ['$1', '$3']).
expr -> expr '!=' expr             : op('$2', ['$1', '$3']).
expr -> kw_case expr '{' case_clauses '}' : case_expr('$1', '$2', '$4').
expr -> kw_fn '('      ')' '{' exprs '}'  : fn('$1', [], '$5').
expr -> kw_fn '(' args ')' '{' exprs '}'  : fn('$1', '$3', '$6').

case_clauses -> case_clause              : ['$1'].
case_clauses -> case_clause case_clauses : ['$1'|'$2'].

case_clause -> '|' pattern '->' expr : case_clause('$1', '$2', '$4').

% TODO: The hole in call args results in entirely different semantics- partial
% application. Given this do we want to use different AST for the parent node?
% Currently when we do anything with calls we have to check if they have a hole
% or not. This is a bit fiddly.
call_args -> hole               : [hole('$1')].
call_args -> hole ','           : [hole('$1')].
call_args -> hole ',' call_args : [hole('$1') | '$3'].
call_args -> expr               : ['$1'].
call_args -> expr ','           : ['$1'].
call_args -> expr ',' call_args : ['$1' | '$3'].

args -> name          : [arg('$1')].
args -> name ','      : [arg('$1')].
args -> name ',' args : [arg('$1') | '$3'].

elems -> expr           : ['$1'].
elems -> expr ','       : ['$1'].
elems -> expr ',' elems : ['$1' | '$3'].

fields -> field             : ['$1'].
fields -> field ','         : ['$1'].
fields -> field ',' fields  : ['$1' | '$3'].

field -> name '=' expr      : record_field('$1', '$3').

pattern -> literal                      : '$1'.
pattern -> name                         : var('$1').
pattern -> hole                         : hole('$1').
pattern -> pattern '::' pattern         : cons('$2', '$1', '$3').
pattern -> upname                       : enum_constructor('$1', []).
pattern -> upname '(' elems_pattern ')' : enum_constructor('$1', '$3').
pattern -> '(' ')'                      : tuple('$1', []).
pattern -> '(' elems_pattern ')'        : tuple('$1', '$2').
pattern -> '{' elems_pattern '}'        : tuple('$1', '$2').
pattern -> '[' ']'                      : list('$2', []).
pattern -> '[' elems_pattern ']'        : list('$3', '$2').

elems_pattern -> pattern                   : ['$1'].
elems_pattern -> pattern ','               : ['$1'].
elems_pattern -> pattern ',' elems_pattern : ['$1' | '$3'].

literal -> atom              : literal('$1').
literal -> int               : literal('$1').
literal -> float             : literal('$1').
literal -> string            : literal('$1').

Erlang code.

-include("gleam_records.hrl").

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

op({Operator, Meta}, Args) ->
  #ast_operator{meta = Meta, name = atom_to_list(Operator), args = Args}.

local_call({'(', Meta}, Fn, Args) ->
  #ast_call{meta = Meta, fn = Fn, args = Args}.

fn_call({'.', Meta}, Fn, Args) ->
  #ast_fn_call{meta = Meta, fn = Fn, args = Args}.

function(Public, {name, Meta, Name}, Args, Body) ->
  #ast_mod_fn{public = Public,
              meta = Meta,
              name = Name,
              args = Args,
              body = Body}.

enum(Public, {upname, Meta, Name}, Args, Constructors) ->
  #ast_mod_enum{meta = Meta,
                public = Public,
                name = Name,
                args = Args,
                constructors = Constructors}.

enum_def({upname, Meta, Name}, Args) ->
  #ast_enum_def{meta = Meta,
                name = Name,
                args = Args}.

type_constructor({upname, Meta, Name}, Args) ->
  #ast_type_constructor{meta = Meta,
                        name = Name,
                        args = Args}.

type_var({name, Meta, Name}) ->
  #ast_type_var{meta = Meta,
                name = Name}.

assignment({'=', Meta}, {name, _, Name}, Value, Then) ->
  #ast_assignment{meta = Meta, name = Name, value = Value, then = Then}.

arg({name, _Meta, Name}) ->
  Name.

var({name, Meta, Name}) ->
  #ast_var{meta = Meta, name = Name}.

record({'{', EmptyMeta}, Fields) ->
  record_extend(#ast_record_empty{meta = EmptyMeta}, Fields).

record_extend(Record, Fields) ->
  Extend = fun({Label, Value}, Parent) ->
    Meta = #meta{} = element(2, Value), % HACK
    #ast_record_extend{meta = Meta,
                       label = Label,
                       value = Value,
                       parent = Parent}
  end,
  lists:foldl(Extend, Record, lists:reverse(Fields)).

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

enum_constructor({upname, Meta, Name}, Elems) ->
  #ast_enum{name = Name, meta = Meta, elems = Elems}.

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
