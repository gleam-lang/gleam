-module(gleam_parser_test).
-include_lib("eunit/include/eunit.hrl").
-include("gleam_records.hrl").

tokens(Code)  -> element(2, gleam_tokenizer:string(Code)).
parse(Tokens) -> element(2, gleam_parser:parse(Tokens)).

-define(assertAST(Code, Tokens),
        ?assertMatch(Tokens, parse(tokens(Code)))).

literal_test() ->
  ?assertAST("1",   [#gleam_ast_int{value = 1}]),
  ?assertAST("1.2", [#gleam_ast_float{value = 1.2}]),
  ?assertAST(":ok", [#gleam_ast_atom{value = ok}]),
  ?assertAST("\"Hello world\"", [#gleam_ast_string{value = <<"Hello world">>}]).

var_test() ->
  ?assertAST("value", [#gleam_ast_var{name = value}]).

tuple_test() ->
  ?assertAST("()",
             [#gleam_ast_tuple{elems = []}]),
  ?assertAST("(:54)",
             [#gleam_ast_tuple{elems = [#gleam_ast_atom{value = '54'}]}]),
  ?assertAST("(  200,)",
             [#gleam_ast_tuple{elems = [#gleam_ast_int{value = 200}]}]),
  ?assertAST("(:ok, 7)",
             [#gleam_ast_tuple{elems = [#gleam_ast_atom{value = ok}
                                       ,#gleam_ast_int{value = 7}]}]).

module_test() ->
  ?assertAST(
    "module MyModule\n"
    "fn id(x) = x\n"
    "fn ok(val) = (:ok, val)\n",
    #gleam_ast_module
    { name = 'MyModule'
    , functions =
      [ #gleam_ast_function
        { name = id
        , args = [x]
        , body = [#gleam_ast_var{name = x}]
        }
      , #gleam_ast_function
        { name = ok
        , args = [val]
        , body =
          [ #gleam_ast_tuple
            {elems =
              [ #gleam_ast_atom{value = ok}
              , #gleam_ast_var{name = val}
              ]
            }
          ]
        }
      ]
    }
  ).

arity_2_test() ->
  ?assertAST(
    "module MyModule\n"
    "fn add(x, y) = x + y\n",
    #gleam_ast_module
    { name = 'MyModule'
    , functions =
      [  #gleam_ast_function
        { name = add
        , args = [x, y]
        , body = [#gleam_ast_local_call{name = '+'}]
        }
      ]
    }
  ).

call_test() ->
  Code =
    "module MyModule\n"
    "fn run() = print(20)\n"
  ,
  AST =
    #gleam_ast_module
    { name = 'MyModule'
    , functions =
      [  #gleam_ast_function
        { name = run
        , args = []
        , body =
          [ #gleam_ast_local_call
            { name = print
            , args = [#gleam_ast_int{value = 20, line = 2}]
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
    #gleam_ast_module
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
    #gleam_ast_module
    { name = 'MyModule'
    , functions =
      [ #gleam_ast_function
        { name = ok
        , args = []
        , body =
          [ #gleam_ast_adt
            { name = 'Ok'
            , line = 2
            , elems = [#gleam_ast_int{line = 2, value = 1}]
            }
          ]
        }
      ]
    },
  ?assertEqual(AST, parse(tokens(Code))).
