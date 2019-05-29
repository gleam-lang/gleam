-module(std@result_test).
-compile(no_auto_import).

-export([is_ok_test/0, is_error_test/0, map_test/0, map_error_test/0, flatten_test/0, then_test/0, unwrap_test/0]).

is_ok_test() ->
    std@expect:true(std@result:is_ok({ok, 1})),
    std@expect:false(std@result:is_ok({error, 1})).

is_error_test() ->
    std@expect:false(std@result:is_error({ok, 1})),
    std@expect:true(std@result:is_error({error, 1})).

map_test() ->
    std@expect:equal(std@result:map({ok, 1}, fun(X) -> X + 1 end), {ok, 2}),
    std@expect:equal(std@result:map({ok, 1}, fun(_) -> <<"2">> end),
                     {ok, <<"2">>}),
    std@expect:equal(std@result:map({error, 1}, fun(X) -> X + 1 end),
                     {error, 1}).

map_error_test() ->
    std@expect:equal(std@result:map_error({ok, 1}, fun(X) -> X + 1 end),
                     {ok, 1}),
    std@expect:equal(std@result:map_error({error, 1}, fun(X) -> X + 1 end),
                     {error, 2}).

flatten_test() ->
    std@expect:equal(std@result:flatten({ok, {ok, 1}}), {ok, 1}),
    std@expect:equal(std@result:flatten({ok, {error, 1}}), {error, 1}),
    std@expect:equal(std@result:flatten({error, 1}), {error, 1}),
    std@expect:equal(std@result:flatten({error, {error, 1}}),
                     {error, {error, 1}}).

then_test() ->
    std@expect:equal(std@result:then({error, 1}, fun(X) -> {ok, X + 1} end),
                     {error, 1}),
    std@expect:equal(std@result:then({ok, 1}, fun(X) -> {ok, X + 1} end),
                     {ok, 2}),
    std@expect:equal(std@result:then({ok, 1},
                                     fun(_) -> {ok, <<"type change">>} end),
                     {ok, <<"type change">>}),
    std@expect:equal(std@result:then({ok, 1}, fun(_) -> {error, 1} end),
                     {error, 1}).

unwrap_test() ->
    std@expect:equal(std@result:unwrap({ok, 1}, 50), 1),
    std@expect:equal(std@result:unwrap({error, <<"nope">>}, 50), 50).
