-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).

-record(
  gleam_ast_module
  , { name = undefined
    , exports = []
    , functions = []
    }
  ).

-record(
  gleam_ast_function
  , { name = undefined
    , args = []
    , body = []
    }
  ).

-record(
  gleam_ast_call
  , { module = undefined
    , name = undefined
    , args = []
    }
  ).

-record(
  gleam_ast_local_call
  , { name = undefined
    , args = []
    }
  ).

-record(
  gleam_ast_assignment
  , { name = undefined
    , value = undefined
    , then = undefined
    }
  ).

-record(gleam_ast_int,    { line = undefined, value = undefined }).
-record(gleam_ast_float,  { line = undefined, value = undefined }).
-record(gleam_ast_bool,   { line = undefined, value = undefined }).
-record(gleam_ast_atom,   { line = undefined, value = undefined }).
-record(gleam_ast_string, { line = undefined, value = undefined }).
-record(gleam_ast_var,    { line = undefined, name = undefined }).
-record(gleam_ast_tuple,  { elems = [] }).
-record(gleam_ast_list,   { elems = [] }).
