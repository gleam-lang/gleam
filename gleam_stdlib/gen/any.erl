-module(any).
-compile(no_auto_import).
-include_lib("eunit/include/eunit.hrl").

-export([from/1, unsafeCoerce/1, string/1, int/1, float/1, bool/1, thunk/1, list/2, tuple/1, field/2]).

list_module() ->
    list.

tuple_module() ->
    tuple.

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
    gleam__stdlib:decode_thunk(A).

-ifdef(TEST).
thunk_test() ->
    expect:is_ok(thunk(from(fun() -> 1 end))),
    expect:equal(result:map(thunk(from(fun() -> 1 end)), fun(F) -> F() end),
                 {ok, from(1)}),
    expect:is_error(thunk(from(fun(X) -> X end))),
    expect:is_error(thunk(from(1))),
    expect:is_error(thunk(from([]))).
-endif.

list_any(A) ->
    gleam__stdlib:decode_list(A).

list(Any, Decode) ->
    result:then(list_any(Any),
                fun(Capture1) ->
                    (list_module()):traverse(Capture1, Decode)
                end).

-ifdef(TEST).
list_test() ->
    expect:equal(list(from([]), fun string/1), {ok, []}),
    expect:equal(list(from([]), fun int/1), {ok, []}),
    expect:equal(list(from([1, 2, 3]), fun int/1), {ok, [1, 2, 3]}),
    expect:equal(list(from([[1], [2], [3]]),
                      fun(Capture1) -> list(Capture1, fun int/1) end),
                 {ok, [[1], [2], [3]]}),
    expect:is_error(list(from(1), fun string/1)),
    expect:is_error(list(from(1.0), fun int/1)),
    expect:is_error(list(from([<<"">>]), fun int/1)),
    expect:is_error(list(from([from(1), from(<<"not an int">>)]), fun int/1)).
-endif.

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
                                         fun(X) ->
                                             result:map(int((tuple_module()):first(X)),
                                                        fun(F) ->
                                                            {F,
                                                             (tuple_module()):second(X)}
                                                        end)
                                         end),
                             fun(X) ->
                                 result:map(float((tuple_module()):second(X)),
                                            fun(F) ->
                                                {(tuple_module()):first(X), F}
                                            end)
                             end),
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
