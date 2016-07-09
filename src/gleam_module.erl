-module(gleam_module).
-include("gleam_records.hrl").

-export([from_ast/1]).

from_ast(AST) ->
  try lists:foldl(fun fold/2, #gleam_module{}, AST) of
    #gleam_module{ name = undefined } ->
      {error, missing_module_statement};

    Mod ->
      {ok, Mod}
  catch
    {gleam_error, E} ->
      {error, E}
  end.


%%% Internal functions

fold({module, _, N}, #gleam_module{ name = undefined } = Mod) ->
  Mod#gleam_module{ name = N };

fold({module, _, _}, _) ->
  throw({gleam_error, duplicate_module_statement});

fold(_, Mod) ->
  Mod.
