-define(print(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).

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
        {meta = #meta{},
         module = undefined,
         name = undefined,
         args = []}).

-record(ast_cons,
        {meta = #meta{},
         head = undefined,
         tail = undefined}).

-record(ast_local_call,
        {meta = #meta{},
         name = undefined,
         args = []}).

-record(ast_assignment,
        {meta = #meta{},
         name = undefined,
         value = undefined,
         then = undefined}).

-record(ast_adt,
        {meta = #meta{},
         name = undefined,
         elems = []}).

-record(ast_case,
        {meta = #meta{},
         subject = undefined,
         clauses = []}).

-record(ast_clause,
        {meta = #meta{},
         pattern = undefined,
         value = undefined}).

-record(ast_record, {meta = #meta{}, fields = []}).

-record(ast_record_field,
        {meta = #meta{},
         key = undefined,
         value = undefined}).

-record(ast_record_access,
        {meta = #meta{},
         record = undefined,
         key = undefined}).

-record(ast_tuple,  {meta = #meta{}, elems = []}).
-record(ast_list,   {meta = #meta{}, elems = []}).

-record(ast_int,    {meta = #meta{}, value = undefined}).
-record(ast_float,  {meta = #meta{}, value = undefined}).
-record(ast_bool,   {meta = #meta{}, value = undefined}).
-record(ast_atom,   {meta = #meta{}, value = undefined}).
-record(ast_string, {meta = #meta{}, value = undefined}).
-record(ast_var,    {meta = #meta{}, name = undefined}).
