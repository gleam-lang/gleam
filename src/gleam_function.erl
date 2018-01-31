-module(gleam_function).
-include("gleam_records.hrl").

-export([from_ast/1]).

from_ast(_) -> ok.

% from_ast({function, _, Name, RawClauses}) ->
%   case clauses(RawClauses) of
%     {ok, Arity, Body} ->
%       Func = #gleam_function
%       { name = Name
%       , arity = Arity
%       , body = Body
%       },
%       {ok, Func};

%     {gleam_error, E} ->
%       {error, E}
%   end.


% %% Private functions

% clauses(Clauses) ->
%   clauses(Clauses, undefined, []).

% clauses([{def, _Meta, Args, Body}|Tail], undefined, Acc) ->
%   Arity = length(Args),
%   Clauses = [{Args, Body}|Acc],
%   clauses(Tail, Arity, Clauses);

% clauses([{def, _Meta, Args, Body}|Tail], Arity, Acc) ->
%   if
%     length(Args) == Arity ->
%       Clauses = [{Args, Body}|Acc],
%       clauses(Tail, Arity, Clauses);

%     true ->
%       {gleam_error, arity_mismatch}
%   end;

% clauses([], Arity, Acc) ->
%   {ok, Arity, lists:reverse(Acc)}.
