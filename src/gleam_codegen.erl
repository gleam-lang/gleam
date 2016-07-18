-module(gleam_codegen).
-include("gleam_records.hrl").

-export([module/1]).

module(#gleam_module{} = Mod) ->
  Name = cerl:c_atom(Mod#gleam_module.name),
  Exports = [],
  Functions = [],
  Core = cerl:c_module(Name, Exports, [], Functions),
  {ok, Core}.
