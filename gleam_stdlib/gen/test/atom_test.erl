-module(atom_test).
-compile(no_auto_import).

-export([from_string_test/0, create_from_string_test/0, to_string_test/0]).

from_string_test() ->
    expect:is_ok(atom:from_string(<<"ok">>)),
    expect:is_ok(atom:from_string(<<"expect">>)),
    expect:is_error(atom:from_string(<<"this is not an atom we have seen before">>)).

create_from_string_test() ->
    expect:equal({ok, atom:create_from_string(<<"ok">>)},
                 atom:from_string(<<"ok">>)),
    expect:equal({ok, atom:create_from_string(<<"expect">>)},
                 atom:from_string(<<"expect">>)),
    expect:equal({ok,
                  atom:create_from_string(<<"this is another atom we have not seen before">>)},
                 atom:from_string(<<"this is another atom we have not seen before">>)).

to_string_test() ->
    expect:equal(atom:to_string(atom:create_from_string(<<"ok">>)), <<"ok">>),
    expect:equal(atom:to_string(atom:create_from_string(<<"expect">>)),
                 <<"expect">>).
