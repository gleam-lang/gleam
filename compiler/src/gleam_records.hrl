-define(print(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).

-record(meta, {line = 1 :: non_neg_integer()}).
-type meta() :: #meta{}.

-type export() :: {string(), non_neg_integer()}.
-type type_annotation() :: type_not_annotated | {ok, type()}.

-record(ast_tuple,
        {meta = #meta{} :: meta(),
         elems = [] :: [ast_expression()]}).

-record(ast_list,
        {meta = #meta{} :: meta(),
         elems = [] :: [ast_expression()]}).

-record(ast_int,
        {meta = #meta{} :: #meta{},
         value :: integer()}).

-record(ast_float,
        {meta = #meta{} :: meta(),
         value :: float()}).

-record(ast_atom,
        {meta = #meta{} :: meta(),
         value :: string()}).

-record(ast_string,
        {meta = #meta{} :: meta(),
         value :: binary()}).

-record(ast_var,
        {meta = #meta{},
         type = type_not_annotated :: type_annotation(),
         name :: string()}).

-record(ast_closure,
        {meta = #meta{} :: meta(),
         type = type_not_annotated :: type_annotation(),
         args = [] :: [string()],
         body :: ast_expression()}).

-record(ast_call,
        {meta = #meta{} :: meta(),
         module :: string(),
         name :: string(),
         args = [] :: [ast_expression()]}).

-record(ast_cons,
        {meta = #meta{} :: meta(),
         head :: ast_expression(),
         tail :: ast_expression()}).

-record(ast_closure_call,
        {meta = #meta{} :: meta(),
         closure :: ast_expression(),
         args = [] :: [ast_expression()]}).

-record(ast_raise,
        {meta = #meta{} :: meta(),
         value :: ast_expression()}).

-record(ast_throw,
        {meta = #meta{} :: meta(),
         value :: ast_expression()}).

% TODO: It sucks that the local call assumes that the local call
% assumes that there is a variable name like so `my_fun()`, this
% means we can't do things like this `get_fun()()` without writing
% it as `x = get_fun(); x()`
-record(ast_local_call,
        {meta = #meta{} :: meta(),
         type = type_not_annotated :: type_annotation(),
         name :: string(),
         args = [] :: [ast_expression()]}).

-record(ast_assignment,
        {meta = #meta{} :: meta(),
         name :: string(),
         value :: ast_expression(),
         then :: ast_expression()}).

-record(ast_adt,
        {meta = #meta{} :: meta(),
         name :: string(),
         elems = [] :: [ast_expression()]}).

-record(ast_clause,
        {meta = #meta{} :: meta(),
         pattern :: ast_expression(),
         value :: ast_expression()}).

-record(ast_case,
        {meta = #meta{} :: meta(),
         subject :: ast_expression(),
         clauses = [#ast_clause{}]}).

-record(ast_record_field,
        {meta = #meta{} :: meta(),
         key :: string(),
         value :: ast_expression()}).

-record(ast_record,
        {meta = #meta{} :: meta(),
         fields = [#ast_record_field{}]}).

-record(ast_record_access,
        {meta = #meta{} :: meta(),
         record :: ast_expression(),
         key :: string()}).

-record(ast_pipe,
        {meta = #meta{} :: meta(),
         lhs :: ast_expression(),
         rhs :: ast_expression()}).

-record(ast_function,
        {meta = #meta{} :: meta(),
         name :: string(),
         args = [] :: [string()],
         body :: ast_expression()}).

-record(ast_test,
        {meta = #meta{} :: meta(),
         name :: string(),
         body :: ast_expression()}).

-record(ast_module,
        {name = "" :: string(),
         exports = [] :: [export()],
         functions = [] :: [#ast_function{}],
         tests = [] :: [#ast_test{}]}).

-record(ast_seq,
        {first :: ast_expression(),
         then :: ast_expression()}).

-type ast_expression()
      :: #ast_adt{}
      | #ast_assignment{}
      | #ast_atom{}
      | #ast_call{}
      | #ast_case{}
      | #ast_closure_call{}
      | #ast_closure{}
      | #ast_cons{}
      | #ast_float{}
      | #ast_int{}
      | #ast_list{}
      | #ast_local_call{}
      | #ast_pipe{}
      | #ast_raise{}
      | #ast_record_access{}
      | #ast_record{}
      | #ast_string{}
      | #ast_throw{}
      | #ast_tuple{}
      | #ast_var{}
      | #ast_seq{}.

%
% Types
%

-type id() :: reference().
-type type_var_reference() :: reference().
-type level() :: integer().
-type const_type() :: int | float | atom | string.

-record(type_const, {type :: const_type()}).
-record(type_tuple, {elems :: [type()]}).
-record(type_func, {args :: list(type()), return :: type()}).
% -record(type_app, {type :: type(), args :: list(type())}).
% TODO: Refine this type. Should be one type for with the id, one with the ref.
% See the Gleam implementation for details.
-record(type_var, {type :: type_var_reference() | id()}).

-type type() :: #type_const{} | #type_tuple{} | #type_func{} | #type_var{}.

%
% Type variables
%

-record(type_var_unbound, {id :: id(), level :: level()}).
-record(type_var_link, {type :: type()}).
-record(type_var_generic, {id :: id()}).

-type type_var() :: #type_var_unbound{} | #type_var_link{} | #type_var_generic{}.
