-module(gleam@result_test).
-compile(no_auto_import).

-export([is_ok_test/0, is_error_test/0, map_test/0, map_error_test/0, flatten_test/0, then_test/0, unwrap_test/0]).

is_ok_test() ->
    gleam@expect:true(gleam@result:is_ok({ok, 1})),
    gleam@expect:false(gleam@result:is_ok({error, 1})).

is_error_test() ->
    gleam@expect:false(gleam@result:is_error({ok, 1})),
    gleam@expect:true(gleam@result:is_error({error, 1})).

map_test() ->
    gleam@expect:equal(gleam@result:map({ok, 1}, fun(X) -> X + 1 end), {ok, 2}),
    gleam@expect:equal(gleam@result:map({ok, 1}, fun(_) -> <<"2">> end),
                       {ok, <<"2">>}),
    gleam@expect:equal(gleam@result:map({error, 1}, fun(X) -> X + 1 end),
                       {error, 1}).

map_error_test() ->
    gleam@expect:equal(gleam@result:map_error({ok, 1}, fun(X) -> X + 1 end),
                       {ok, 1}),
    gleam@expect:equal(gleam@result:map_error({error, 1}, fun(X) -> X + 1 end),
                       {error, 2}).

flatten_test() ->
    gleam@expect:equal(gleam@result:flatten({ok, {ok, 1}}), {ok, 1}),
    gleam@expect:equal(gleam@result:flatten({ok, {error, 1}}), {error, 1}),
    gleam@expect:equal(gleam@result:flatten({error, 1}), {error, 1}),
    gleam@expect:equal(gleam@result:flatten({error, {error, 1}}),
                       {error, {error, 1}}).

then_test() ->
    gleam@expect:equal(gleam@result:then({error, 1}, fun(X) -> {ok, X + 1} end),
                       {error, 1}),
    gleam@expect:equal(gleam@result:then({ok, 1}, fun(X) -> {ok, X + 1} end),
                       {ok, 2}),
    gleam@expect:equal(gleam@result:then({ok, 1},
                                         fun(_) -> {ok, <<"type change">>} end),
                       {ok, <<"type change">>}),
    gleam@expect:equal(gleam@result:then({ok, 1}, fun(_) -> {error, 1} end),
                       {error, 1}).

unwrap_test() ->
    gleam@expect:equal(gleam@result:unwrap({ok, 1}, 50), 1),
    gleam@expect:equal(gleam@result:unwrap({error, <<"nope">>}, 50), 50).
