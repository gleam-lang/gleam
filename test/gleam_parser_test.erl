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

% list_test() ->
%   ?assertAST("[]",        [[]]),
%   ?assertAST("[100]",     [[100]]),
%   ?assertAST("[  200,]",  [[200]]),
%   ?assertAST("[2, 4, 8]", [[2, 4, 8]]),
%   ?assertAST("[self()]",  [[{self, _, []}]]),
%   ?assertAST("[2,10, ]",  [[2, 10]]).

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

% nesting_test() ->
%   ?assertAST("[(), ()]", [[{}, {}]]),
%   ?assertAST("[[], []]", [[[], []]]),
%   ?assertAST("((), ())", [{{}, {}}]),
%   ?assertAST("[(:ok, 1), (:error, [()])]",
%              [[{ok, 1}, {error, [{}]}]]).

% call_test() ->
%   ?assertAST("self()",
%              [{self, [{line, 1}], []}]),
%   ?assertAST("list_to_tuple([1, 2, 3])",
%              [{list_to_tuple, _, [[1, 2, 3]]}]),
%   ?assertAST("add(4, 3, 2)",
%              [{add, _, [4, 3, 2]}]),
%   ?assertAST("parse(tokens(:ok))",
%              [{parse, _, [{tokens, _, [ok]}]}]).

% mod_call_test() ->
%   ?assertAST("erlang.time()",
%              [{'.', [{line, 1}], [erlang, time], []}]),
%   ?assertAST("gen_server.module_info()",
%              [{'.', _, [gen_server, module_info], []}]),
%   ?assertAST("lists.max([5, 10])",
%              [{'.', _, [lists, max], [[5, 10]]}]).

% assignment_test() ->
%   ?assertAST("x = 1",
%              [{'=', _, [x, 1]}]),
%   ?assertAST("my_var = wrangle(555)",
%              [{'=', _, [my_var, {wrangle, _, [555]}]}]).

% multiple_statement_test() ->
%   ?assertAST("print(1)print(2)",
%              [{print, _, [1]}, {print, _, [2]}]),
%   ?assertAST("print(1)\nprint(2)",
%              [{print, _, [1]}, {print, _, [2]}]),
%   ?assertAST("1 2 3",
%              [1, 2, 3]).

% function_test() ->
%   ?assertAST("let id(x) { x }",
%              [{function, _, id, [{variable, _, x}], [{variable, _, x}]}]),
%   ?assertAST("let wrap(x) { [x] }",
%              [{function, _, wrap, [{variable, _, x}], [[{variable, _, x}]]}]),
%   ?assertAST("let inspect(e) { print(e) e }",
%              [{function, _, inspect, [{variable, _, e}],
%                  [{print, _, [{variable, _, e}]}, {variable, _, e}]}]).

module_test() ->
  ?assertAST(
    "module MyModule\n"
    "let id(x) = x\n"
    "let ok(val) = (:ok, val)\n",
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
