-module(gleam_hello_world).

-export([not/1]).

not(B) ->
    'true' |> fun bool:not/1.
