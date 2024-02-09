-module(mist_ffi).

-export([binary_match/2, decode_packet/3, file_open/1, string_to_int/2]).

decode_packet(Type, Packet, Opts) ->
  case erlang:decode_packet(Type, Packet, Opts) of
    {ok, http_eoh, Rest} ->
      {ok, {end_of_headers, Rest}};
    {ok, Binary, Rest} ->
      {ok, {binary_data, Binary, Rest}};
    {more, Length} when Length =:= undefined ->
      {ok, {more_data, none}};
    {more, Length} ->
      {ok, {more_data, {some, Length}}};
    {error, Reason} ->
      {error, Reason}
  end.

binary_match(Source, Pattern) ->
  case binary:match(Source, Pattern) of
    {Before, After} ->
      {ok, {Before, After}};
    nomatch ->
      {error, nil}
  end.

string_to_int(String, Base) ->
  try
    {ok, erlang:list_to_integer(String, Base)}
  catch
    badarg ->
      {error, nil}
  end.

file_open(Path) ->
  case file:open(Path, [raw]) of
    {ok, fd} ->
      {ok, fd};
    {error, enoent} ->
      {error, no_entry};
    {error, eacces} ->
      {error, no_access};
    {error, eisdir} ->
      {error, is_dir};
    _ ->
      {error, unknown_file_error}
  end.
