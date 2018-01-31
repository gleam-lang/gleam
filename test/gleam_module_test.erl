-module(gleam_module_test).
-include_lib("eunit/include/eunit.hrl").
-include("../src/gleam_records.hrl").

%module_statement_test() ->
%  AST = [{module, [{line, 1}], hello}],
%  {ok, Mod} = gleam_module:from_ast(AST),
%  Expected = #gleam_module
%  { name = hello
%  },
%  ?assertEqual(Expected, Mod).

%%% We return an error when a module AST does not include
%%% the `module Name` statement
%%%
%missing_module_statement_test() ->
%  Result = gleam_module:from_ast([]),
%  ?assertEqual({error, missing_module_statement}, Result).

%%% We return an error when a module AST includes multiple
%%% the `module Name` statement
%%%
%dupe_module_statement_test() ->
%  AST = [{module, [{line, 1}], hello},
%         {module, [{line, 1}], world}],
%  Result = gleam_module:from_ast(AST),
%  ?assertEqual({error, duplicate_module_statement}, Result).

%%% Top level expressions are discarded.
%%%
%expression_discarding_test() ->
%  AST = [{module, [{line, 1}], expr},
%         1, "hello", {print, [], [123]}],
%  {ok, Mod} = gleam_module:from_ast(AST),
%  Expected = #gleam_module
%  { name = expr
%  },
%  ?assertEqual(Expected, Mod).

%private_function_test() ->
%  AST = [{module, [{line, 1}], expr},
%         {function, [{line, 1}], private, one,
%          [{def, [], [1], [1]}]}],
%  {ok, Mod} = gleam_module:from_ast(AST),
%  Expected = #gleam_module
%  { name = expr
%  , exports = []
%  , functions =
%    [ #gleam_function
%      { name = one
%      , arity = 1
%      , publicity = private
%      , clauses = [{[1], [1]}]
%      }
%    ]
%  },
%  ?assertEqual(Expected, Mod).

%public_function_test() ->
%  AST = [{module, [{line, 1}], expr},
%         {function, [{line, 1}], public, one,
%          [{def, [], [1], [1]}]}],
%  {ok, Mod} = gleam_module:from_ast(AST),
%  Expected = #gleam_module
%  { name = expr
%  , exports = [{one, 1}]
%  , functions =
%    [ #gleam_function
%      { name = one
%      , arity = 1
%      , publicity = public
%      , clauses = [{[1], [1]}]
%      }
%    ]
%  },
%  ?assertEqual(Expected, Mod).
