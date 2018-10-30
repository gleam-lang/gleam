-module(gleam__decode_erl).

-export([any/1, string/1, int/1, float/1, atom/1, bool/1, pid/1, reference/1,
         thunk/1, tuple/1, tuple3/1, tuple4/1, tuple5/1, field/2]).

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

bool(Data) when is_boolean(Data) -> {ok, Data};
bool(Data) -> err("a Bool", Data).

pid(Data) when is_pid(Data) -> {ok, Data};
pid(Data) -> err("a Pid", Data).

reference(Data) when is_reference(Data) -> {ok, Data};
reference(Data) -> err("a Reference", Data).

thunk(Data) when is_function(Data, 0) -> {ok, Data};
thunk(Data) -> err("a zero arity function", Data).

tuple(Data = {_, _}) -> {ok, Data};
tuple(Data) -> err("a 2 element tuple", Data).

tuple3(Data = {_, _, _}) -> {ok, Data};
tuple3(Data) -> err("a 3 element tuple", Data).

tuple4(Data = {_, _, _, _}) -> {ok, Data};
tuple4(Data) -> err("a 4 element tuple", Data).

tuple5(Data = {_, _, _, _, _}) -> {ok, Data};
tuple5(Data) -> err("a 5 element tuple", Data).

field(Data, Key) ->
  case Data of
    #{Key := Value} ->
      {ok, Value};

    _ ->
      err(io_lib:format("a map with key `~p`", [Key]), Data)
  end.
