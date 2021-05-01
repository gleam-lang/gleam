-module(test).
-compile(no_auto_import).

-export([test/2, suite/2, pass/0, fail/1, test_equal/3, equal/2, detail/2, run/2]).
-export_type([test/0, pass/0, fail/0, stats/0]).

-opaque test() :: {test, binary(), fun(() -> {ok, pass()} | {error, fail()})} |
    {suite, binary(), list(test())}.

-opaque pass() :: pass.

-opaque fail() :: {fail, binary()}.

-type stats() :: {stats, integer(), integer()}.

-spec test(binary(), fun(() -> {ok, pass()} | {error, fail()})) -> test().
test(Name, Proc) ->
    {test, Name, Proc}.

-spec suite(binary(), list(test())) -> test().
suite(Name, Tests) ->
    {suite, Name, Tests}.

-spec pass() -> {ok, pass()} | {error, fail()}.
pass() ->
    {ok, pass}.

-spec fail(binary()) -> {ok, pass()} | {error, fail()}.
fail(Detail) ->
    {error, {fail, Detail}}.

-spec test_equal(binary(), N, N) -> test().
test_equal(Name, Left, Right) ->
    {test, Name, fun() -> equal(Left, Right) end}.

-spec equal(O, O) -> {ok, pass()} | {error, fail()}.
equal(Left, Right) ->
    case Left =:= Right of
        true ->
            pass();

        false ->
            fail(<<"Values were not equal"/utf8>>)
    end.

-spec detail({ok, pass()} | {error, fail()}, binary()) -> {ok, pass()} |
    {error, fail()}.
detail(Outcome, Detail) ->
    case Outcome of
        {ok, pass} ->
            pass();

        {error, _} ->
            fail(Detail)
    end.

-spec run(list(test()), fun((binary()) -> binary())) -> stats().
run(Tests, Print) ->
    run_list_of_tests(Tests, Print, {stats, 0, 0}, 0).

-spec run_test(test(), fun((binary()) -> binary()), stats(), integer()) -> stats().
run_test(Test, Print, Stats, Indentation) ->
    case Test of
        {test, Name, Proc} ->
            run_single_test(Name, Proc, Print, Stats, Indentation);

        {suite, Name@1, Tests} ->
            run_suite(Name@1, Tests, Print, Stats, Indentation)
    end.

-spec run_suite(
    binary(),
    list(test()),
    fun((binary()) -> binary()),
    stats(),
    integer()
) -> stats().
run_suite(Name, Tests, Print, Stats, Indentation) ->
    print_indentation(Print, Indentation),
    Print(Name),
    Print(<<"\n"/utf8>>),
    run_list_of_tests(Tests, Print, Stats, Indentation + 1).

-spec run_list_of_tests(
    list(test()),
    fun((binary()) -> binary()),
    stats(),
    integer()
) -> stats().
run_list_of_tests(Tests, Print, Stats, Indentation) ->
    case Tests of
        [] ->
            Stats;

        [Test | Tests@1] ->
            Stats@1 = run_test(Test, Print, Stats, Indentation),
            run_list_of_tests(Tests@1, Print, Stats@1, Indentation)
    end.

-spec run_single_test(
    binary(),
    fun(() -> {ok, pass()} | {error, fail()}),
    fun((binary()) -> binary()),
    stats(),
    integer()
) -> stats().
run_single_test(Name, Proc, Print, Stats, Indentation) ->
    print_indentation(Print, Indentation),
    Print(Name),
    Print(<<": "/utf8>>),
    case Proc() of
        {ok, pass} ->
            Print(<<"✨"/utf8>>),
            Print(<<"\n"/utf8>>),
            erlang:setelement(2, Stats, erlang:element(2, Stats) + 1);

        {error, {fail, Detail}} ->
            Print(<<"❌ "/utf8>>),
            Print(Detail),
            Print(<<"\n"/utf8>>),
            erlang:setelement(3, Stats, erlang:element(3, Stats) + 1)
    end.

-spec print_indentation(fun((binary()) -> binary()), integer()) -> nil.
print_indentation(Print, Indentation) ->
    case Indentation > 0 of
        true ->
            Print(<<"  "/utf8>>),
            print_indentation(Print, Indentation - 1);

        false ->
            nil
    end.
