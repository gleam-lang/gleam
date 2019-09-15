-module(gleam@any_test).
-compile(no_auto_import).

-export([string_test/0, int_test/0, float_test/0, thunk_test/0, bool_test/0, atom_test/0, list_test/0, struct2_test/0, field_test/0]).

string_test() ->
    gleam@expect:equal(gleam@any:string(gleam@any:from(<<"">>)), {ok, <<"">>}),
    gleam@expect:equal(
        gleam@any:string(gleam@any:from(<<"Hello">>)),
        {ok, <<"Hello">>}
    ),
    gleam@expect:equal(
        gleam@any:string(gleam@any:from(1)),
        {error, <<"Expected a String, got `1`">>}
    ),
    gleam@expect:equal(
        gleam@any:string(gleam@any:from([])),
        {error, <<"Expected a String, got `[]`">>}
    ).

int_test() ->
    gleam@expect:equal(gleam@any:int(gleam@any:from(1)), {ok, 1}),
    gleam@expect:equal(gleam@any:int(gleam@any:from(2)), {ok, 2}),
    gleam@expect:equal(
        gleam@any:int(gleam@any:from(1.0)),
        {error, <<"Expected an Int, got `1.0`">>}
    ),
    gleam@expect:equal(
        gleam@any:int(gleam@any:from([])),
        {error, <<"Expected an Int, got `[]`">>}
    ).

float_test() ->
    gleam@expect:equal(gleam@any:float(gleam@any:from(1.0)), {ok, 1.0}),
    gleam@expect:equal(gleam@any:float(gleam@any:from(2.2)), {ok, 2.2}),
    gleam@expect:equal(
        gleam@any:float(gleam@any:from(1)),
        {error, <<"Expected a Float, got `1`">>}
    ),
    gleam@expect:equal(
        gleam@any:float(gleam@any:from([])),
        {error, <<"Expected a Float, got `[]`">>}
    ).

thunk_test() ->
    gleam@expect:is_ok(gleam@any:thunk(gleam@any:from(fun() -> 1 end))),
    gleam@expect:equal(
        gleam@result:map(
            gleam@any:thunk(gleam@any:from(fun() -> 1 end)),
            fun(F) -> F() end
        ),
        {ok, gleam@any:from(1)}
    ),
    gleam@expect:is_error(gleam@any:thunk(gleam@any:from(fun(X) -> X end))),
    gleam@expect:is_error(gleam@any:thunk(gleam@any:from(1))),
    gleam@expect:is_error(gleam@any:thunk(gleam@any:from([]))).

bool_test() ->
    gleam@expect:equal(gleam@any:bool(gleam@any:from(true)), {ok, true}),
    gleam@expect:equal(gleam@any:bool(gleam@any:from(false)), {ok, false}),
    gleam@expect:equal(
        gleam@any:bool(gleam@any:from(1)),
        {error, <<"Expected a Bool, got `1`">>}
    ),
    gleam@expect:equal(
        gleam@any:bool(gleam@any:from([])),
        {error, <<"Expected a Bool, got `[]`">>}
    ).

atom_test() ->
    gleam@expect:equal(
        gleam@any:atom(gleam@any:from(gleam@atom:create_from_string(<<"">>))),
        {ok, gleam@atom:create_from_string(<<"">>)}
    ),
    gleam@expect:equal(
        gleam@any:atom(gleam@any:from(gleam@atom:create_from_string(<<"ok">>))),
        {ok, gleam@atom:create_from_string(<<"ok">>)}
    ),
    gleam@expect:is_error(gleam@any:atom(gleam@any:from(1))),
    gleam@expect:is_error(gleam@any:atom(gleam@any:from([]))).

list_test() ->
    gleam@expect:equal(
        gleam@any:list(gleam@any:from([]), fun gleam@any:string/1),
        {ok, []}
    ),
    gleam@expect:equal(
        gleam@any:list(gleam@any:from([]), fun gleam@any:int/1),
        {ok, []}
    ),
    gleam@expect:equal(
        gleam@any:list(gleam@any:from([1, 2, 3]), fun gleam@any:int/1),
        {ok, [1, 2, 3]}
    ),
    gleam@expect:equal(
        gleam@any:list(
            gleam@any:from([[1], [2], [3]]),
            fun(Capture1) -> gleam@any:list(Capture1, fun gleam@any:int/1) end
        ),
        {ok, [[1], [2], [3]]}
    ),
    gleam@expect:is_error(
        gleam@any:list(gleam@any:from(1), fun gleam@any:string/1)
    ),
    gleam@expect:is_error(
        gleam@any:list(gleam@any:from(1.0), fun gleam@any:int/1)
    ),
    gleam@expect:is_error(
        gleam@any:list(gleam@any:from([<<"">>]), fun gleam@any:int/1)
    ),
    gleam@expect:is_error(
        gleam@any:list(
            gleam@any:from(
                [gleam@any:from(1), gleam@any:from(<<"not an int">>)]
            ),
            fun gleam@any:int/1
        )
    ).

struct2_test() ->
    gleam@expect:equal(
        gleam@any:struct2(gleam@any:from({1, []})),
        {ok, {gleam@any:from(1), gleam@any:from([])}}
    ),
    gleam@expect:equal(
        gleam@any:struct2(gleam@any:from({<<"ok">>, <<"ok">>})),
        {ok, {gleam@any:from(<<"ok">>), gleam@any:from(<<"ok">>)}}
    ),
    gleam@expect:is_error(gleam@any:struct2(gleam@any:from({1}))),
    gleam@expect:is_error(gleam@any:struct2(gleam@any:from({1, 2, 3}))),
    gleam@expect:equal(
        gleam@result:then(
            gleam@result:then(
                gleam@any:struct2(gleam@any:from({1, 2.0})),
                fun(X) ->
                    gleam@result:map(
                        gleam@any:int(gleam@tuple:first(X)),
                        fun(F) -> {F, gleam@tuple:second(X)} end
                    )
                end
            ),
            fun(X) ->
                gleam@result:map(
                    gleam@any:float(gleam@tuple:second(X)),
                    fun(F) -> {gleam@tuple:first(X), F} end
                )
            end
        ),
        {ok, {1, 2.0}}
    ).

field_test() ->
    {ok, OkAtom} = gleam@atom:from_string(<<"ok">>),
    {ok, EarlierAtom} = gleam@atom:from_string(<<"earlier">>),
    gleam@expect:equal(
        gleam@any:field(
            gleam@any:from(gleam@map:put(gleam@map:new(), OkAtom, 1)),
            OkAtom
        ),
        {ok, gleam@any:from(1)}
    ),
    gleam@expect:equal(
        gleam@any:field(
            gleam@any:from(
                gleam@map:put(
                    gleam@map:put(gleam@map:new(), OkAtom, 3),
                    EarlierAtom,
                    1
                )
            ),
            OkAtom
        ),
        {ok, gleam@any:from(3)}
    ),
    gleam@expect:is_error(
        gleam@any:field(gleam@any:from(gleam@map:new()), OkAtom)
    ),
    gleam@expect:is_error(gleam@any:field(gleam@any:from(1), OkAtom)),
    gleam@expect:is_error(gleam@any:field(gleam@any:from([]), [])).
