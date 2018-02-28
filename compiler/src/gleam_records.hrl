-define(print(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).

-record(meta, {line = 1 :: non_neg_integer()}).

-type export() :: {string(), non_neg_integer()}.

-record(ast_tuple,  {meta = #meta{}, elems = [] :: [ast_expression()]}).
-record(ast_list,   {meta = #meta{}, elems = [] :: [ast_expression()]}).

-record(ast_int,    {meta = #meta{} :: #meta{}, value :: integer()}).
-record(ast_float,  {meta = #meta{} :: #meta{}, value :: float()}).
-record(ast_bool,   {meta = #meta{} :: #meta{}, value :: boolean()}).
-record(ast_atom,   {meta = #meta{} :: #meta{}, value :: string()}).
-record(ast_string, {meta = #meta{} :: #meta{}, value :: binary()}).

-record(ast_var, {meta = #meta{}, name :: string()}).

-record(ast_closure,
        {meta = #meta{} :: #meta{},
         args = [] :: [string()],
         body :: ast_expression()}).

-record(ast_call,
        {meta = #meta{} :: #meta{},
         module :: string(),
         name :: string(),
         args = [] :: [ast_expression()]}).

-record(ast_cons,
        {meta = #meta{} :: #meta{},
         head :: ast_expression(),
         tail :: ast_expression()}).

-record(ast_closure_call,
        {meta = #meta{} :: #meta{},
         closure :: ast_expression(),
         args = [] :: [ast_expression()]}).

-record(ast_raise,
        {meta = #meta{} :: #meta{},
         value :: ast_expression()}).

-record(ast_local_call,
        {meta = #meta{} :: #meta{},
         name :: string(),
         args = [] :: [ast_expression()]}).

-record(ast_assignment,
        {meta = #meta{} :: #meta{},
         name :: string(),
         value :: ast_expression(),
         then :: ast_expression()}).

-record(ast_adt,
        {meta = #meta{} :: #meta{},
         name :: string(),
         elems = [] :: [ast_expression()]}).

-record(ast_clause,
        {meta = #meta{} :: #meta{},
         pattern :: ast_expression(),
         value :: ast_expression()}).

-record(ast_case,
        {meta = #meta{} :: #meta{},
         subject :: ast_expression(),
         clauses = [#ast_clause{}]}).

-record(ast_record_field,
        {meta = #meta{} :: #meta{},
         key :: string(),
         value :: ast_expression()}).

-record(ast_record,
        {meta = #meta{} :: #meta{},
         fields = [#ast_record_field{}]}).

-record(ast_record_access,
        {meta = #meta{} :: #meta{},
         record :: ast_expression(),
         key :: string()}).

-record(ast_pipe,
        {meta = #meta{} :: #meta{},
         lhs :: ast_expression(),
         rhs :: ast_expression()}).

-record(ast_function,
        {meta = #meta{} :: #meta{},
         name :: string(),
         args = [] :: [string()],
         body :: ast_expression()}).

-record(ast_test,
        {meta = #meta{} :: #meta{},
         name :: string(),
         body :: ast_expression()}).

-record(ast_module,
        {name = "" :: string(),
         exports = [] :: [export()],
         functions = [] :: [#ast_function{}],
         tests = [] :: [#ast_test{}]}).

-type ast_expression()
      :: #ast_adt{}
      | #ast_assignment{}
      | #ast_atom{}
      | #ast_bool{}
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
      | #ast_tuple{}
      | #ast_var{}.
