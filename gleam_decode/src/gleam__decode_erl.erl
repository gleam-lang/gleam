-module(gleam__decode_erl).

-export([string/1, int/1, float/1, atom/1]).

err(Type, Data) ->
  {error, iolist_to_binary(io_lib:format("Expected ~s, got `~p`", [Type, Data]))}.

any(Data) -> {ok, Data}.

string(Data) when is_binary(Data) -> {ok, Data};
string(Data) -> err("a String", Data).

int(Data) when is_integer(Data) -> {ok, Data};
int(Data) -> err("an Int", Data).

float(Data) when is_float(Data) -> {ok, Data};
float(Data) -> err("a Float", Data).

atom(Data) when is_atom(Data) -> {ok, Data};
atom(Data) -> err("an Atom", Data).
