-module(any).
-compile(no_auto_import).
-include_lib("eunit/include/eunit.hrl").

-export([from/1, unsafeCoerce/1, string/1, int/1, float/1, bool/1, thunk/1, list/2, tuple/1, field/2]).

from(A) ->
    gleam__stdlib:identity(A).

unsafeCoerce(A) ->
    gleam__stdlib:identity(A).

string(A) ->
    gleam__stdlib:decode_string(A).

-ifdef(TEST).
string_test() ->
    expect:equal(string(from(<<"">>)), {ok, <<"">>}),
    expect:equal(string(from(<<"Hello">>)), {ok, <<"Hello">>}),
    expect:equal(string(from(1)), {error, <<"Expected a String, got `1`">>}),
    expect:equal(string(from([])), {error, <<"Expected a String, got `[]`">>}).
-endif.

int(A) ->
    gleam__stdlib:decode_int(A).

-ifdef(TEST).
int_test() ->
    expect:equal(int(from(1)), {ok, 1}),
    expect:equal(int(from(2)), {ok, 2}),
    expect:equal(int(from(1.0)), {error, <<"Expected an Int, got `1.0`">>}),
    expect:equal(int(from([])), {error, <<"Expected an Int, got `[]`">>}).
-endif.

float(A) ->
    gleam__stdlib:decode_float(A).

-ifdef(TEST).
float_test() ->
    expect:equal(float(from(1.0)), {ok, 1.0}),
    expect:equal(float(from(2.2)), {ok, 2.2}),
    expect:equal(float(from(1)), {error, <<"Expected a Float, got `1`">>}),
    expect:equal(float(from([])), {error, <<"Expected a Float, got `[]`">>}).
-endif.

bool(A) ->
    gleam__stdlib:decode_bool(A).

-ifdef(TEST).
bool_test() ->
    expect:equal(bool(from(true)), {ok, true}),
    expect:equal(bool(from(false)), {ok, false}),
    expect:equal(bool(from(1)), {error, <<"Expected a Bool, got `1`">>}),
    expect:equal(bool(from([])), {error, <<"Expected a Bool, got `[]`">>}).
-endif.

thunk(A) ->
    gleam__stdlib:thunk(A).

list_any(A) ->
    gleam__stdlib:decode_list(A).

list_module() ->
    list.

list(Any, Decode) ->
    result:then(list_any(Any),
                fun(X) -> (list_module()):traverse(X, Decode) end).

tuple(A) ->
    gleam__stdlib:decode_tuple(A).

-ifdef(TEST).
tuple_test() ->
    expect:equal(tuple(from({1, []})), {ok, {from(1), from([])}}),
    expect:equal(tuple(from({<<"ok">>, <<"ok">>})),
                 {ok, {from(<<"ok">>), from(<<"ok">>)}}),
    expect:is_error(tuple(from({1}))),
    expect:is_error(tuple(from({1, 2, 3}))),
    expect:equal(result:then(result:then(tuple(from({1, 2.0})),
                                         fun(X) -> {A, B} = X,
                                             result:map(int(A),
                                                        fun(I) ->
                                                            {I, B}
                                                        end) end),
                             fun(X) -> {A1, B1} = X,
                                 result:map(float(B1),
                                            fun(F) -> {A1, F} end) end),
                 {ok, {1, 2.0}}).
-endif.

field(A, B) ->
    gleam__stdlib:decode_field(A, B).

-ifdef(TEST).
field_test() ->
    {ok, OkAtom} = atom:from_string(<<"ok">>),
    expect:equal(field(from(#{}#{ok => 1}), OkAtom), {ok, from(1)}),
    expect:equal(field(from(#{}#{ok => 3}#{earlier => 2}), OkAtom),
                 {ok, from(3)}),
    expect:is_error(field(from(#{}), OkAtom)),
    expect:is_error(field(from(1), OkAtom)),
    expect:is_error(field(from([]), [])).
-endif.
