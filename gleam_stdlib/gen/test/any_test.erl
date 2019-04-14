-module(any_test).
-compile(no_auto_import).

-export([string_test/0, int_test/0, float_test/0, thunk_test/0, bool_test/0, atom_test/0, list_test/0, tuple_test/0, field_test/0]).

string_test() ->
    expect:equal(any:string(any:from(<<"">>)), {ok, <<"">>}),
    expect:equal(any:string(any:from(<<"Hello">>)), {ok, <<"Hello">>}),
    expect:equal(any:string(any:from(1)),
                 {error, <<"Expected a String, got `1`">>}),
    expect:equal(any:string(any:from([])),
                 {error, <<"Expected a String, got `[]`">>}).

int_test() ->
    expect:equal(any:int(any:from(1)), {ok, 1}),
    expect:equal(any:int(any:from(2)), {ok, 2}),
    expect:equal(any:int(any:from(1.0)),
                 {error, <<"Expected an Int, got `1.0`">>}),
    expect:equal(any:int(any:from([])),
                 {error, <<"Expected an Int, got `[]`">>}).

float_test() ->
    expect:equal(any:float(any:from(1.0)), {ok, 1.0}),
    expect:equal(any:float(any:from(2.2)), {ok, 2.2}),
    expect:equal(any:float(any:from(1)),
                 {error, <<"Expected a Float, got `1`">>}),
    expect:equal(any:float(any:from([])),
                 {error, <<"Expected a Float, got `[]`">>}).

thunk_test() ->
    expect:is_ok(any:thunk(any:from(fun() -> 1 end))),
    expect:equal(result:map(any:thunk(any:from(fun() -> 1 end)),
                            fun(F) -> F() end),
                 {ok, any:from(1)}),
    expect:is_error(any:thunk(any:from(fun(X) -> X end))),
    expect:is_error(any:thunk(any:from(1))),
    expect:is_error(any:thunk(any:from([]))).

bool_test() ->
    expect:equal(any:bool(any:from(true)), {ok, true}),
    expect:equal(any:bool(any:from(false)), {ok, false}),
    expect:equal(any:bool(any:from(1)),
                 {error, <<"Expected a Bool, got `1`">>}),
    expect:equal(any:bool(any:from([])),
                 {error, <<"Expected a Bool, got `[]`">>}).

atom_test() ->
    expect:equal(any:atom(any:from(atom:create_from_string(<<"">>))),
                 {ok, atom:create_from_string(<<"">>)}),
    expect:equal(any:atom(any:from(atom:create_from_string(<<"ok">>))),
                 {ok, atom:create_from_string(<<"ok">>)}),
    expect:is_error(any:atom(any:from(1))),
    expect:is_error(any:atom(any:from([]))).

list_test() ->
    expect:equal(any:list(any:from([]), fun any:string/1), {ok, []}),
    expect:equal(any:list(any:from([]), fun any:int/1), {ok, []}),
    expect:equal(any:list(any:from([1, 2, 3]), fun any:int/1), {ok, [1, 2, 3]}),
    expect:equal(any:list(any:from([[1], [2], [3]]),
                          fun(Capture1) ->
                              any:list(Capture1, fun any:int/1)
                          end),
                 {ok, [[1], [2], [3]]}),
    expect:is_error(any:list(any:from(1), fun any:string/1)),
    expect:is_error(any:list(any:from(1.0), fun any:int/1)),
    expect:is_error(any:list(any:from([<<"">>]), fun any:int/1)),
    expect:is_error(any:list(any:from([any:from(1), any:from(<<"not an int">>)]),
                             fun any:int/1)).

tuple_test() ->
    expect:equal(any:tuple(any:from({1, []})),
                 {ok, {any:from(1), any:from([])}}),
    expect:equal(any:tuple(any:from({<<"ok">>, <<"ok">>})),
                 {ok, {any:from(<<"ok">>), any:from(<<"ok">>)}}),
    expect:is_error(any:tuple(any:from({1}))),
    expect:is_error(any:tuple(any:from({1, 2, 3}))),
    expect:equal(result:then(result:then(any:tuple(any:from({1, 2.0})),
                                         fun(X) ->
                                             result:map(any:int(tuple:first(X)),
                                                        fun(F) ->
                                                            {F, tuple:second(X)}
                                                        end)
                                         end),
                             fun(X) ->
                                 result:map(any:float(tuple:second(X)),
                                            fun(F) -> {tuple:first(X), F} end)
                             end),
                 {ok, {1, 2.0}}).

field_test() ->
    {ok, OkAtom} = atom:from_string(<<"ok">>),
    expect:equal(any:field(any:from(#{}#{ok => 1}), OkAtom), {ok, any:from(1)}),
    expect:equal(any:field(any:from(#{}#{ok => 3}#{earlier => 2}), OkAtom),
                 {ok, any:from(3)}),
    expect:is_error(any:field(any:from(#{}), OkAtom)),
    expect:is_error(any:field(any:from(1), OkAtom)),
    expect:is_error(any:field(any:from([]), [])).
