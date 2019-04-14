-module(result_test).
-compile(no_auto_import).

-export([is_ok_test/0, is_error_test/0, map_test/0, map_error_test/0, flatten_test/0, then_test/0, unwrap_test/0]).

is_ok_test() ->
    expect:true(result:is_ok({ok, 1})),
    expect:false(result:is_ok({error, 1})).

is_error_test() ->
    expect:false(result:is_error({ok, 1})),
    expect:true(result:is_error({error, 1})).

map_test() ->
    expect:equal(result:map({ok, 1}, fun(X) -> X + 1 end), {ok, 2}),
    expect:equal(result:map({ok, 1}, fun(_) -> <<"2">> end), {ok, <<"2">>}),
    expect:equal(result:map({error, 1}, fun(X) -> X + 1 end), {error, 1}).

map_error_test() ->
    expect:equal(result:map_error({ok, 1}, fun(X) -> X + 1 end), {ok, 1}),
    expect:equal(result:map_error({error, 1}, fun(X) -> X + 1 end), {error, 2}).

flatten_test() ->
    expect:equal(result:flatten({ok, {ok, 1}}), {ok, 1}),
    expect:equal(result:flatten({ok, {error, 1}}), {error, 1}),
    expect:equal(result:flatten({error, 1}), {error, 1}),
    expect:equal(result:flatten({error, {error, 1}}), {error, {error, 1}}).

then_test() ->
    expect:equal(result:then({error, 1}, fun(X) -> {ok, X + 1} end),
                 {error, 1}),
    expect:equal(result:then({ok, 1}, fun(X) -> {ok, X + 1} end), {ok, 2}),
    expect:equal(result:then({ok, 1}, fun(_) -> {ok, <<"type change">>} end),
                 {ok, <<"type change">>}),
    expect:equal(result:then({ok, 1}, fun(_) -> {error, 1} end), {error, 1}).

unwrap_test() ->
    expect:equal(result:unwrap({ok, 1}, 50), 1),
    expect:equal(result:unwrap({error, <<"nope">>}, 50), 50).
