-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).

-record(meta, {line = undefined}).

-record(ast_module,
        {name = undefined,
         exports = [],
         functions = []}).

-record(ast_function,
        {meta = #meta{},
         name = undefined,
         args = [],
         body = []}).

-record(ast_call,
        {module = undefined,
         name = undefined,
         args = []}).

-record(ast_local_call,
        {name = undefined,
         args = []}).

-record(ast_assignment,
        {name = undefined,
         value = undefined,
         then = undefined}).

-record(ast_adt,
        {meta = #meta{},
         name = undefined,
         elems = []}).

-record(ast_case,
        {subject = undefined,
         clauses = []}).

-record(ast_clause,
        {pattern = undefined,
         value = undefined}).

-record(ast_int,    {meta = #meta{}, value = undefined}).
-record(ast_float,  {meta = #meta{}, value = undefined}).
-record(ast_bool,   {meta = #meta{}, value = undefined}).
-record(ast_atom,   {meta = #meta{}, value = undefined}).
-record(ast_string, {meta = #meta{}, value = undefined}).
-record(ast_var,    {meta = #meta{}, name = undefined}).
-record(ast_tuple,  {elems = []}).
-record(ast_list,   {elems = []}).
