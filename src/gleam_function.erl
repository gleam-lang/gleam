-module(gleam_function).
-include("gleam_records.hrl").

-export([from_ast/1]).

from_ast({function, _, Publicity, Name, RawClauses})
  when Publicity == public; Publicity == private ->
  case clauses(RawClauses) of
    {ok, Arity, Clauses} ->
      Func = #gleam_function
      { name = Name
      , arity = Arity
      , publicity = Publicity
      , clauses = Clauses
      },
      {ok, Func};

    {gleam_error, E} ->
      {error, E}
  end.


%% Private functions

clauses(Clauses) ->
  clauses(Clauses, undefined, []).


clauses([{def, Args, Body}|Tail], undefined, Acc) ->
  Arity = tuple_size(Args),
  Clauses = [{Args, Body}|Acc],
  clauses(Tail, Arity, Clauses);

clauses([{def, Args, Body}|Tail], Arity, Acc) ->
  if
    tuple_size(Args) == Arity ->
      Clauses = [{Args, Body}|Acc],
      clauses(Tail, Arity, Clauses);

    true ->
      {gleam_error, arity_mismatch}
  end;

clauses([], Arity, Acc) ->
  {ok, Arity, lists:reverse(Acc)}.
