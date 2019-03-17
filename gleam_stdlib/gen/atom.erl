-module(atom).
-compile(no_auto_import).
-include_lib("eunit/include/eunit.hrl").

-export([from_string/1, create_from_string/1, to_string/1]).

from_string(A) ->
    gleam__stdlib:atom_from_string(A).

-ifdef(TEST).
from_string_test() ->
    expect:is_ok(from_string(<<"ok">>)),
    expect:is_ok(from_string(<<"expect">>)),
    expect:equal(from_string(<<"this is not an atom we have seen before">>),
                 {error, atom_not_loaded}).
-endif.

create_from_string(A) ->
    gleam__stdlib:atom_create_from_string(A).

-ifdef(TEST).
create_from_string_test() ->
    Ok = fun(X) -> {ok, X} end,
    expect:equal(Ok(create_from_string(<<"ok">>)), from_string(<<"ok">>)),
    expect:equal(Ok(create_from_string(<<"expect">>)),
                 from_string(<<"expect">>)),
    expect:equal(Ok(create_from_string(<<"this is another atom we have not seen before">>)),
                 from_string(<<"this is another atom we have not seen before">>)).
-endif.

to_string(A) ->
    gleam__stdlib:atom_to_string(A).

-ifdef(TEST).
to_string_test() ->
    expect:equal(to_string(create_from_string(<<"ok">>)), <<"ok">>),
    expect:equal(to_string(create_from_string(<<"expect">>)), <<"expect">>).
-endif.
