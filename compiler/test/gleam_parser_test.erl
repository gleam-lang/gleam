-module(gleam_parser_test).
-include_lib("eunit/include/eunit.hrl").
-include("gleam_records.hrl").

tokens(Code)  -> element(2, gleam_tokenizer:string(Code)).
parse(Tokens) -> element(2, gleam_parser:parse(Tokens)).

-define(assertAST(Code, Tokens),
        ?assertMatch(Tokens, parse(tokens(Code)))).

literal_test() ->
  ?assertAST("1",   #ast_int{value = 1}),
  ?assertAST("1.2", #ast_float{value = 1.2}),
  ?assertAST("'ok'", #ast_atom{value = "ok"}),
  ?assertAST("\"Hello world\"", #ast_string{value = <<"Hello world">>}).

var_test() ->
  ?assertAST("value", #ast_var{name = "value"}).

tuple_test() ->
  ?assertAST("{'54'}",
             #ast_tuple{meta = #meta{line = 1},
                        elems = [#ast_atom{value = "54"}]}),
  ?assertAST("{\n  200,}",
             #ast_tuple{meta = #meta{line = 1},
                        elems = [#ast_int{value = 200}]}),
  ?assertAST("{'ok', 7}",
             #ast_tuple{meta = #meta{line = 1},
                        elems = [#ast_atom{value = "ok"},
                                 #ast_int{value = 7}]}).

module_test() ->
  Code =
    "module MyModule\n"
    "fn id(x) { x }\n"
    "fn ok(val) { {'ok', val} }\n"
  ,
  AST =
    #ast_module
    { name = "MyModule"
    , tests = []
    , functions =
      [ #ast_mod_fn
        { meta = #meta{line = 2}
        , name = "id"
        , args = ["x"]
        , body = #ast_var{name = "x", meta = #meta{line = 2}}
        }
      , #ast_mod_fn
        { meta = #meta{line = 3}
        , name = "ok"
        , args = ["val"]
        , body =
          #ast_tuple
          {meta = #meta{line = 3},
           elems =
            [ #ast_atom{meta = #meta{line = 3}, value = "ok"}
            , #ast_var{meta = #meta{line = 3}, name = "val"}
            ]
          }
        }
      ]
    },
  ?assertEqual(AST, parse(tokens(Code))).

arity_2_test() ->
  Code =
    "module MyModule\n"
    "fn add(x, y) { x + y }\n"
  ,
  AST =
    #ast_module
    { name = "MyModule"
    , tests = []
    , functions =
      [ #ast_mod_fn
        { meta = #meta{line = 2}
        , name = "add"
        , args = ["x", "y"]
        , body =
          #ast_operator
          { meta = #meta{line = 2}
          , name = "+"
          , args =
            [ #ast_var{meta = #meta{line = 2}, name = "x"}
            , #ast_var{meta = #meta{line = 2}, name = "y"}
            ]
          }
        }
      ]
    },
  ?assertEqual(AST, parse(tokens(Code))).

call_test() ->
  Code =
    "module MyModule\n"
    "fn run() { print(20) }\n"
  ,
  AST =
    #ast_module
    { name = "MyModule"
    , tests = []
    , functions =
      [ #ast_mod_fn
        { meta = #meta{line = 2}
        , name = "run"
        , args = []
        , body =
          #ast_local_call
          { meta = #meta{line = 2}
          , fn = #ast_var{meta = #meta{line = 2}, name = "print"}
          , args =
            [#ast_int{meta = #meta{line = 2}, value = 20}]
          }
        }
      ]
    },
  ?assertEqual(AST, parse(tokens(Code))).

pub_test() ->
  ?assertAST(
    "module MyModule\n"
    "pub fn id(x) { x }\n",
    #ast_module
    { name = "MyModule"
    , tests = []
    , exports = [{"id", 1}]
    , functions =
      [ #ast_mod_fn
        { meta = #meta{line = 2}
        , name = "id"
        , args = ["x"]
        , body =
          #ast_var
          { meta = #meta{line = 2}
          , name = "x"
          }
        }
      ]
    }
  ).

adt_test() ->
  Code =
    "module MyModule\n"
    "fn ok() { Ok(1) }"
  ,
  AST =
    #ast_module
    { name = "MyModule"
    , tests = []
    , functions =
      [ #ast_mod_fn
        { meta = #meta{line = 2}
        , name = "ok"
        , args = []
        , body =
          #ast_adt
          { meta = #meta{line = 2}
          , name = "Ok"
          , elems = [#ast_int{meta = #meta{line = 2}, value = 1}]
          }
        }
      ]
    },
  ?assertEqual(AST, parse(tokens(Code))).

test_test() ->
  Code =
    "module MyModule\n"
    "test ok { 'ok' }"
  ,
  AST =
    #ast_module
    { name = "MyModule"
    , tests =
      [#ast_mod_test{meta = #meta{line = 2},
                     name = "ok",
                     body = #ast_atom{meta = #meta{line = 2}, value = "ok"}}]
    , functions = []
    },
  ?assertEqual(AST, parse(tokens(Code))).

sequence_test() ->
  Code =
    "1 2 3"
  ,
  AST =
    #ast_seq{first = #ast_int{value = 1},
             then = #ast_seq{first = #ast_int{value = 2},
                             then = #ast_int{value = 3}}}
  ,
  ?assertEqual(AST, parse(tokens(Code))).

fn_test() ->
  Code =
    "module MyModule\n"
    "fn thunk(x) { fn() { x } }"
    "fn make_identity() { fn(x) { x } }"
  ,
  AST =
    #ast_module
    { name = "MyModule"
    , tests = []
    , functions =
      [ #ast_mod_fn
        { meta = #meta{line = 2}
        , name = "thunk"
        , args = ["x"]
        , body =
          #ast_fn
          { meta = #meta{line = 2}
          , args = []
          , body = #ast_var{meta = #meta{line = 2}, name = "x"}
          }
        },
        #ast_mod_fn
        { meta = #meta{line = 2}
        , name = "make_identity"
        , args = []
        , body =
          #ast_fn
          { meta = #meta{line = 2}
          , args = ["x"]
          , body = #ast_var{meta = #meta{line = 2}, name = "x"}
          }
        }
      ]
    },
  ?assertEqual(AST, parse(tokens(Code))).

curried_test() ->
  Code = "f(1)(2)",
  AST =
    #ast_local_call
    { fn =
      #ast_local_call
      { fn = #ast_var{name = "f"}
      , args = [#ast_int{value = 1}]
      }
    , args = [#ast_int{value = 2}]
    },
  ?assertEqual(AST, parse(tokens(Code))),
  Code2 = "f.(1)(2)",
  AST2 =
    #ast_local_call
    { fn =
      #ast_fn_call
      { fn = #ast_var{name = "f"}
      , args = [#ast_int{value = 1}]
      }
    , args = [#ast_int{value = 2}]
    },
  ?assertEqual(AST2, parse(tokens(Code2))).
