-module(bool).
-compile(no_auto_import).
-include_lib("eunit/include/eunit.hrl").

-export([negate/1, max/2, min/2, to_int/1]).

negate(Bool) ->
    case Bool of
        true ->
            false;

        false ->
            true
    end.

-ifdef(TEST).
negate_test() ->
    expect:false(negate(true)),
    expect:true(negate(false)).
-endif.

max(A, B) ->
    case A of
        true ->
            true;

        false ->
            B
    end.

-ifdef(TEST).
max_test() ->
    expect:equal(max(true, true), true),
    expect:equal(max(true, false), true),
    expect:equal(max(false, false), false),
    expect:equal(max(false, true), true).
-endif.

min(A, B) ->
    case A of
        false ->
            false;

        true ->
            B
    end.

-ifdef(TEST).
min_test() ->
    expect:equal(min(true, true), true),
    expect:equal(min(true, false), false),
    expect:equal(min(false, false), false),
    expect:equal(min(false, true), false).
-endif.

to_int(Bool) ->
    case Bool of
        false ->
            0;

        true ->
            1
    end.

-ifdef(TEST).
to_int_test() ->
    expect:equal(to_int(true), 1),
    expect:equal(to_int(false), 0).
-endif.
