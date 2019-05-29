-module(std@atom_test).
-compile(no_auto_import).

-export([from_string_test/0, create_from_string_test/0, to_string_test/0]).

from_string_test() ->
    std@expect:is_ok(std@atom:from_string(<<"ok">>)),
    std@expect:is_ok(std@atom:from_string(<<"expect">>)),
    std@expect:is_error(std@atom:from_string(<<"this is not an atom we have seen before">>)).

create_from_string_test() ->
    std@expect:equal({ok, std@atom:create_from_string(<<"ok">>)},
                     std@atom:from_string(<<"ok">>)),
    std@expect:equal({ok, std@atom:create_from_string(<<"expect">>)},
                     std@atom:from_string(<<"expect">>)),
    std@expect:equal({ok,
                      std@atom:create_from_string(<<"this is another atom we have not seen before">>)},
                     std@atom:from_string(<<"this is another atom we have not seen before">>)).

to_string_test() ->
    std@expect:equal(std@atom:to_string(std@atom:create_from_string(<<"ok">>)),
                     <<"ok">>),
    std@expect:equal(std@atom:to_string(std@atom:create_from_string(<<"expect">>)),
                     <<"expect">>).
