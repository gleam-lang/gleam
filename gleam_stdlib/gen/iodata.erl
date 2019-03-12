-module(iodata).
-compile(no_auto_import).
-include_lib("eunit/include/eunit.hrl").

-export([prepend/2, append/2, from/1, to_string/1, byte_size/1]).

prepend(A, B) ->
    gleam__stdlib:iodata_prepend(A, B).

append(A, B) ->
    gleam__stdlib:iodata_append(A, B).

from(A) ->
    gleam__stdlib:identity(A).

to_string(A) ->
    erlang:iolist_to_binary(A).

byte_size(A) ->
    erlang:iolist_size(A).

-ifdef(TEST).
iodata_test() ->
    Iodata = (fun(Capture1) ->
        prepend(Capture1, <<"H">>)
    end)((fun(Capture1) ->
             append(Capture1, <<" world!">>)
         end)((fun(Capture1) ->
                  append(Capture1, <<",">>)
              end)(from([<<"ello">>])))),
    (fun(Capture1) ->
        expect:equal(Capture1, <<"Hello, world!">>)
    end)(to_string(Iodata)),
    (fun(Capture1) -> expect:equal(Capture1, 13) end)(byte_size(Iodata)).
-endif.
