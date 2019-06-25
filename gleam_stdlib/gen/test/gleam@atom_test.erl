-module(gleam@atom_test).
-compile(no_auto_import).

-export([from_string_test/0, create_from_string_test/0, to_string_test/0]).

from_string_test() ->
    gleam@expect:is_ok(gleam@atom:from_string(<<"ok">>)),
    gleam@expect:is_ok(gleam@atom:from_string(<<"expect">>)),
    gleam@expect:is_error(gleam@atom:from_string(<<"this is not an atom we have seen before">>)).

create_from_string_test() ->
    gleam@expect:equal({ok, gleam@atom:create_from_string(<<"ok">>)},
                       gleam@atom:from_string(<<"ok">>)),
    gleam@expect:equal({ok, gleam@atom:create_from_string(<<"expect">>)},
                       gleam@atom:from_string(<<"expect">>)),
    gleam@expect:equal({ok,
                        gleam@atom:create_from_string(<<"this is another atom we have not seen before">>)},
                       gleam@atom:from_string(<<"this is another atom we have not seen before">>)).

to_string_test() ->
    gleam@expect:equal(gleam@atom:to_string(gleam@atom:create_from_string(<<"ok">>)),
                       <<"ok">>),
    gleam@expect:equal(gleam@atom:to_string(gleam@atom:create_from_string(<<"expect">>)),
                       <<"expect">>).
