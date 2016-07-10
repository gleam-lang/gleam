-module(gleam_module).
-include("gleam_records.hrl").

-export([from_ast/1]).

from_ast(AST) ->
  case lists:foldl(fun fold/2, #gleam_module{}, AST) of
    #gleam_module{ name = undefined } ->
      {error, missing_module_statement};

    {gleam_error, E} ->
      {error, E};

    Mod ->
      {ok, Mod}
  end.


%%% Internal functions

%% Private function definition
%%
fold({function, _, private, _, _} = FunAST, Mod) ->
  case gleam_function:from_ast(FunAST) of
    {ok, Fun} ->
      Mod#gleam_module{ functions = [Fun|Mod#gleam_module.functions] }
  end;

%% Public function definition
%%
fold({function, _, public, FunName, _} = FunAST, Mod) ->
  case gleam_function:from_ast(FunAST) of
    {ok, Fun} ->
      Export = {FunName, Fun#gleam_function.arity},
      Mod#gleam_module
      { functions = [Fun|Mod#gleam_module.functions]
      , exports = [Export|Mod#gleam_module.exports]
      }
  end;

%% Module statement
%%
fold({module, _, N}, #gleam_module{ name = undefined } = Mod) ->
  Mod#gleam_module{ name = N };

%% Duplicate module statement
%%
fold({module, _, _}, _) ->
  {gleam_error, duplicate_module_statement};

fold(_, Mod) ->
  Mod.
