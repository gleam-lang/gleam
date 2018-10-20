-define(print(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).

-record(meta, {line = 1 :: non_neg_integer()}).
-type meta() :: #meta{}.

-type export() :: {string(), non_neg_integer()}.
-type type_annotation() :: type_not_annotated | {ok, type()}.

-record(ast_enum_def,
        {meta = #meta{} :: meta(),
         name :: string(),
         args = [] :: [ast_type()]}).

-record(ast_type_constructor,
        {meta = #meta{} :: meta(),
         name :: string(),
         args = [] :: [ast_type()]}).

-record(ast_type_var,
        {meta = #meta{} :: meta(),
         name :: string()}).

-type ast_type()
      :: #ast_type_constructor{}
      | #ast_type_var{}
      .

-record(ast_mod_fn,
        {meta = #meta{} :: meta(),
         type = type_not_annotated :: type_annotation(),
         public = false :: boolean(),
         name :: string(),
         args = [] :: [string()],
         body :: ast_expression()}).

-record(ast_mod_test,
        {meta = #meta{} :: meta(),
         name :: string(),
         body :: ast_expression()}).

-record(ast_mod_enum,
        {meta = #meta{} :: meta(),
         public = false :: boolean(),
         args = [] :: [string()],
         name :: string(),
         constructors = [] :: [ast_type()]}).

-type mod_statement()
      :: #ast_mod_fn{}
      | #ast_mod_test{}
      .

-record(ast_tuple,
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

% TODO: Remove the type annotation so it's calculated by traversing the body
% and the args. To do this we will need to turn the args into a list of
% annotated vars rather than strings. We do this in the infer algorithm anyway.
-record(ast_fn,
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

-record(ast_nil,
        {meta = #meta{} :: meta(),
         type = type_not_annotated :: type_annotation()}).

-record(ast_hole,
        {meta = #meta{} :: meta(),
         type = type_not_annotated :: type_annotation()}).

-record(ast_fn_call,
        {meta = #meta{} :: meta(),
         type = type_not_annotated :: type_annotation(),
         fn :: ast_expression(),
         args = [] :: [ast_expression()]}).

-record(ast_raise,
        {meta = #meta{} :: meta(),
         type = type_not_annotated :: type_annotation(),
         value :: ast_expression()}).

-record(ast_throw,
        {meta = #meta{} :: meta(),
         type = type_not_annotated :: type_annotation(),
         value :: ast_expression()}).

-record(ast_local_call,
        {meta = #meta{} :: meta(),
         type = type_not_annotated :: type_annotation(),
         fn :: ast_expression(),
         args = [] :: [ast_expression()]}).

-record(ast_operator,
        {meta = #meta{} :: meta(),
         type = type_not_annotated :: type_annotation(),
         name :: string(),
         args = [] :: [ast_expression()]}).

-record(ast_assignment,
        {meta = #meta{} :: meta(),
         name :: string(),
         value :: ast_expression(),
         then :: ast_expression()}).

-record(ast_enum,
        {meta = #meta{} :: meta(),
         type = type_not_annotated :: type_annotation(),
         name :: string(),
         elems = [] :: [ast_expression()]}). % TODO: Rename to args

-record(ast_clause,
        {meta = #meta{} :: meta(),
         type = type_not_annotated :: type_annotation(),
         pattern :: ast_pattern(),
         value :: ast_expression()}).

-record(ast_case,
        {meta = #meta{} :: meta(),
         type = type_not_annotated :: type_annotation(),
         subject :: ast_expression(),
         clauses = [#ast_clause{}]}).

-record(ast_record_empty,
        {meta = #meta{} :: meta()}).

-record(ast_record_extend,
        {meta = #meta{} :: meta(),
         type = type_not_annotated :: type_annotation(),
         parent :: ast_expression(),
         label :: string(),
         value :: ast_expression()}).

-record(ast_record_select,
        {meta = #meta{} :: meta(),
         type = type_not_annotated :: type_annotation(),
         record :: ast_expression(),
         label :: string()}).

-record(ast_module,
        {type = type_not_annotated :: type_annotation(),
         statements = [] :: [mod_statement()]}).

-record(ast_seq,
        {first :: ast_expression(),
         then :: ast_expression()}).
-type ast_expression()
      :: #ast_enum{}
      | #ast_assignment{}
      | #ast_atom{}
      | #ast_call{}
      | #ast_case{}
      | #ast_cons{}
      | #ast_float{}
      | #ast_fn_call{}
      | #ast_fn{}
      | #ast_hole{}
      | #ast_int{}
      | #ast_local_call{}
      | #ast_nil{}
      | #ast_operator{}
      | #ast_raise{}
      | #ast_record_empty{}
      | #ast_record_extend{}
      | #ast_record_select{}
      | #ast_seq{}
      | #ast_string{}
      | #ast_throw{}
      | #ast_tuple{}
      | #ast_var{}
      .

-type ast_pattern() :: ast_expression(). % TODO: Refine.

%
% Types
%

-type id() :: reference().
-type type_var_reference() :: reference().
-type level() :: integer().

-record(type_const, {type :: string()}).
-record(type_fn, {args :: list(type()), return :: type()}).
-record(type_app, {type :: type(), args :: list(type())}).
-record(type_var, {type :: type_var_reference()}).
-record(type_record, {row :: type()}).
-record(type_module, {row :: type()}).
-record(type_row_empty, {}).
-record(type_row_extend, {label :: string(), type :: type(), parent :: type()}).

-type type()
      :: #type_app{}
      | #type_const{}
      | #type_fn{}
      | #type_var{}
      | #type_record{}
      | #type_module{}
      | #type_row_empty{}
      | #type_row_extend{}
      .

%
% Type variables
%

-record(type_var_unbound, {id :: id(), level :: level()}).
-record(type_var_link, {type :: type()}).
-record(type_var_generic, {id :: id()}).

-type type_var() :: #type_var_unbound{} | #type_var_link{} | #type_var_generic{}.
