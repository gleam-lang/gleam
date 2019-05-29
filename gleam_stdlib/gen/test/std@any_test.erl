-module(std@any_test).
-compile(no_auto_import).

-export([string_test/0, int_test/0, float_test/0, thunk_test/0, bool_test/0, atom_test/0, list_test/0, tuple_test/0, field_test/0]).

string_test() ->
    std@expect:equal(std@any:string(std@any:from(<<"">>)), {ok, <<"">>}),
    std@expect:equal(std@any:string(std@any:from(<<"Hello">>)),
                     {ok, <<"Hello">>}),
    std@expect:equal(std@any:string(std@any:from(1)),
                     {error, <<"Expected a String, got `1`">>}),
    std@expect:equal(std@any:string(std@any:from([])),
                     {error, <<"Expected a String, got `[]`">>}).

int_test() ->
    std@expect:equal(std@any:int(std@any:from(1)), {ok, 1}),
    std@expect:equal(std@any:int(std@any:from(2)), {ok, 2}),
    std@expect:equal(std@any:int(std@any:from(1.0)),
                     {error, <<"Expected an Int, got `1.0`">>}),
    std@expect:equal(std@any:int(std@any:from([])),
                     {error, <<"Expected an Int, got `[]`">>}).

float_test() ->
    std@expect:equal(std@any:float(std@any:from(1.0)), {ok, 1.0}),
    std@expect:equal(std@any:float(std@any:from(2.2)), {ok, 2.2}),
    std@expect:equal(std@any:float(std@any:from(1)),
                     {error, <<"Expected a Float, got `1`">>}),
    std@expect:equal(std@any:float(std@any:from([])),
                     {error, <<"Expected a Float, got `[]`">>}).

thunk_test() ->
    std@expect:is_ok(std@any:thunk(std@any:from(fun() -> 1 end))),
    std@expect:equal(std@result:map(std@any:thunk(std@any:from(fun() -> 1 end)),
                                    fun(F) -> F() end),
                     {ok, std@any:from(1)}),
    std@expect:is_error(std@any:thunk(std@any:from(fun(X) -> X end))),
    std@expect:is_error(std@any:thunk(std@any:from(1))),
    std@expect:is_error(std@any:thunk(std@any:from([]))).

bool_test() ->
    std@expect:equal(std@any:bool(std@any:from(true)), {ok, true}),
    std@expect:equal(std@any:bool(std@any:from(false)), {ok, false}),
    std@expect:equal(std@any:bool(std@any:from(1)),
                     {error, <<"Expected a Bool, got `1`">>}),
    std@expect:equal(std@any:bool(std@any:from([])),
                     {error, <<"Expected a Bool, got `[]`">>}).

atom_test() ->
    std@expect:equal(std@any:atom(std@any:from(std@atom:create_from_string(<<"">>))),
                     {ok, std@atom:create_from_string(<<"">>)}),
    std@expect:equal(std@any:atom(std@any:from(std@atom:create_from_string(<<"ok">>))),
                     {ok, std@atom:create_from_string(<<"ok">>)}),
    std@expect:is_error(std@any:atom(std@any:from(1))),
    std@expect:is_error(std@any:atom(std@any:from([]))).

list_test() ->
    std@expect:equal(std@any:list(std@any:from([]), fun std@any:string/1),
                     {ok, []}),
    std@expect:equal(std@any:list(std@any:from([]), fun std@any:int/1),
                     {ok, []}),
    std@expect:equal(std@any:list(std@any:from([1, 2, 3]), fun std@any:int/1),
                     {ok, [1, 2, 3]}),
    std@expect:equal(std@any:list(std@any:from([[1], [2], [3]]),
                                  fun(Capture1) ->
                                      std@any:list(Capture1, fun std@any:int/1)
                                  end),
                     {ok, [[1], [2], [3]]}),
    std@expect:is_error(std@any:list(std@any:from(1), fun std@any:string/1)),
    std@expect:is_error(std@any:list(std@any:from(1.0), fun std@any:int/1)),
    std@expect:is_error(std@any:list(std@any:from([<<"">>]),
                                     fun std@any:int/1)),
    std@expect:is_error(std@any:list(std@any:from([std@any:from(1),
                                                   std@any:from(<<"not an int">>)]),
                                     fun std@any:int/1)).

tuple_test() ->
    std@expect:equal(std@any:tuple(std@any:from({1, []})),
                     {ok, {std@any:from(1), std@any:from([])}}),
    std@expect:equal(std@any:tuple(std@any:from({<<"ok">>, <<"ok">>})),
                     {ok, {std@any:from(<<"ok">>), std@any:from(<<"ok">>)}}),
    std@expect:is_error(std@any:tuple(std@any:from({1}))),
    std@expect:is_error(std@any:tuple(std@any:from({1, 2, 3}))),
    std@expect:equal(std@result:then(std@result:then(std@any:tuple(std@any:from({1,
                                                                                 2.0})),
                                                     fun(X) ->
                                                         std@result:map(std@any:int(std@tuple:first(X)),
                                                                        fun(F) ->
                                                                            {F,
                                                                             std@tuple:second(X)}
                                                                        end)
                                                     end),
                                     fun(X) ->
                                         std@result:map(std@any:float(std@tuple:second(X)),
                                                        fun(F) ->
                                                            {std@tuple:first(X),
                                                             F}
                                                        end)
                                     end),
                     {ok, {1, 2.0}}).

field_test() ->
    {ok, OkAtom} = std@atom:from_string(<<"ok">>),
    std@expect:equal(std@any:field(std@any:from(#{}#{ok => 1}), OkAtom),
                     {ok, std@any:from(1)}),
    std@expect:equal(std@any:field(std@any:from(#{}#{ok => 3}#{earlier => 2}),
                                   OkAtom),
                     {ok, std@any:from(3)}),
    std@expect:is_error(std@any:field(std@any:from(#{}), OkAtom)),
    std@expect:is_error(std@any:field(std@any:from(1), OkAtom)),
    std@expect:is_error(std@any:field(std@any:from([]), [])).
