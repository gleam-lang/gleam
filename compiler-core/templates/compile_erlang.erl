#!/usr/bin/env escript

% TODO: Don't concurrently print warnings and errors

-record(arguments, {
    lib = "./" :: string(),
    out = "./" :: string(),
    modules = [] :: list(string())
}).

%% # Usage
%%
%% ```shell
%% escript compile_erlang.erl \
%%   --lib path/to/libs \
%%   --out package/ebin \
%%   package/src/one.erl package/src/two.erl
%% ```
%%
main(Args) ->
    #arguments{out = Out, lib = Lib, modules = Modules} = parse(Args),
    ok = add_lib_to_erlang_path(Lib),
    ok = filelib:ensure_dir([Out, $/]),
    Workers = start_compiler_workers(Out),
    ok = producer_loop(Modules, Workers),
    case any_failures() of
        true -> erlang:halt(1);
        false -> ok
    end.

producer_loop([], 0) ->
    ok;
producer_loop([], Workers) ->
    receive
        {work_please, _} -> producer_loop([], Workers - 1)
    end;
producer_loop([Module | Modules], Workers) ->
    receive
        {work_please, Worker} ->
            erlang:send(Worker, {module, Module}),
            producer_loop(Modules, Workers)
    end.

any_failures() ->
    receive
        failed -> true
        after 0 -> false
    end.

start_compiler_workers(Out) ->
    Parent = self(),
    NumSchedulers = erlang:system_info(schedulers),
    SpawnWorker = fun(_) ->
        erlang:spawn_link(fun() ->
            worker_loop(Parent, Out)
        end)
    end,
    lists:foreach(SpawnWorker, lists:seq(1, NumSchedulers)),
    NumSchedulers.

worker_loop(Parent, Out) ->
    Options = [report_errors, report_warnings, debug_info, {outdir, Out}],
    erlang:send(Parent, {work_please, self()}),
    receive
        {module, Module} -> 
            case compile:file(Module, Options) of
                {ok, _} -> ok;
                error -> erlang:send(Parent, failed)
            end,
            worker_loop(Parent, Out)
    end.

add_lib_to_erlang_path(Lib) ->
    Ebins = filelib:wildcard([Lib, "/*/ebin"]),
    ok = code:add_paths(Ebins),
    ok.

parse(Args) ->
    parse(Args, #arguments{}).

parse([], Arguments) ->
    Arguments;
parse(["--lib", Lib | Rest], Arguments) ->
    parse(Rest, Arguments#arguments{lib = Lib});
parse(["--out", Out | Rest], Arguments) ->
    parse(Rest, Arguments#arguments{out = Out});
parse([Module | Rest], Arguments = #arguments{modules = Modules}) ->
    parse(Rest, Arguments#arguments{modules = [Module | Modules]}).
