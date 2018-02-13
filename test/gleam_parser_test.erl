-module(gleam_parser_test).
-include_lib("eunit/include/eunit.hrl").
-include("gleam_records.hrl").

tokens(Code)  -> element(2, gleam_tokenizer:string(Code)).
parse(Tokens) -> element(2, gleam_parser:parse(Tokens)).

-define(assertAST(Code, Tokens),
        ?assertMatch(Tokens, parse(tokens(Code)))).

literal_test() ->
  ?assertAST("1",   [#ast_int{value = 1}]),
  ?assertAST("1.2", [#ast_float{value = 1.2}]),
  ?assertAST(":ok", [#ast_atom{value = ok}]),
  ?assertAST("\"Hello world\"", [#ast_string{value = <<"Hello world">>}]).

var_test() ->
  ?assertAST("value", [#ast_var{name = value}]).

tuple_test() ->
  ?assertAST("()",
             [#ast_tuple{elems = []}]),
  ?assertAST("(:54)",
             [#ast_tuple{elems = [#ast_atom{value = '54'}]}]),
  ?assertAST("(  200,)",
             [#ast_tuple{elems = [#ast_int{value = 200}]}]),
  ?assertAST("(:ok, 7)",
             [#ast_tuple{elems = [#ast_atom{value = ok},
                                  #ast_int{value = 7}]}]).

module_test() ->
  Code =
    "module MyModule\n"
    "fn id(x) = x\n"
    "fn ok(val) = (:ok, val)\n"
  ,
  AST =
    #ast_module
    { name = 'MyModule'
    , functions =
      [ #ast_function
        { meta = #meta{line = 2}
        , name = id
        , args = [x]
        , body = [#ast_var{name = x, meta = #meta{line = 2}}]
        }
      , #ast_function
        { meta = #meta{line = 3}
        , name = ok
        , args = [val]
        , body =
          [ #ast_tuple
            {elems =
              [ #ast_atom{meta = #meta{line = 3}, value = ok}
              , #ast_var{meta = #meta{line = 3}, name = val}
              ]
            }
          ]
        }
      ]
    },
  ?assertEqual(AST, parse(tokens(Code))).

arity_2_test() ->
  Code =
    "module MyModule\n"
    "fn add(x, y) = x + y\n"
  ,
  AST =
    #ast_module
    { name = 'MyModule'
    , functions =
      [ #ast_function
        { meta = #meta{line = 2}
        , name = add
        , args = [x, y]
        , body =
          [ #ast_local_call
            { name = '+'
            , args =
              [ #ast_var{meta = #meta{line = 2}, name = x}
              , #ast_var{meta = #meta{line = 2}, name = y}
              ]
            }
          ]
        }
      ]
    },
  ?assertEqual(AST, parse(tokens(Code))).

call_test() ->
  Code =
    "module MyModule\n"
    "fn run() = print(20)\n"
  ,
  AST =
    #ast_module
    { name = 'MyModule'
    , functions =
      [ #ast_function
        { meta = #meta{line = 2}
        , name = run
        , args = []
        , body =
          [ #ast_local_call
            { name = print
            , args =
              [#ast_int{value = 20, meta = #meta{line = 2}}]
            }
          ]
        }
      ]
    },
  ?assertEqual(AST, parse(tokens(Code))).

export_test() ->
  ?assertAST(
    "module MyModule\n"
    "export id/1\n"
    "export foo/2, baz/8\n",
    #ast_module
    { name = 'MyModule'
    , functions = []
    , exports = [{id, 1}, {foo, 2}, {baz, 8}]
    }
  ).

adt_test() ->
  Code =
    "module MyModule\n"
    "fn ok() = Ok(1)"
  ,
  AST =
    #ast_module
    { name = 'MyModule'
    , functions =
      [ #ast_function
        { meta = #meta{line = 2}
        , name = ok
        , args = []
        , body =
          [ #ast_adt
            { name = 'Ok'
            , meta = #meta{line = 2}
            , elems = [#ast_int{meta = #meta{line = 2}, value = 1}]
            }
          ]
        }
      ]
    },
  ?assertEqual(AST, parse(tokens(Code))).
