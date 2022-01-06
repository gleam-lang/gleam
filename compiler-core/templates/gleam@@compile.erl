#!/usr/bin/env escript

% TODO: Don't concurrently print warnings and errors
% TODO: Some tests

-record(arguments, {lib = "./", out = "./", modules = []}).

main(Args) ->
    #arguments{out = Out, lib = Lib, modules = Modules} = parse(Args),
    ok = configure_logging(),
    ok = add_lib_to_erlang_path(Lib),
    ok = filelib:ensure_dir([Out, $/]),
    Workers = start_compiler_workers(Out),
    ok = producer_loop(Modules, Workers),
    receive
        failed -> erlang:halt(1)
        after 0 -> ok
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

start_compiler_workers(Out) ->
    Parent = self(),
    NumSchedulers = erlang:system_info(schedulers),
    SpawnWorker = fun(_) ->
        erlang:spawn_link(fun() -> worker_loop(Parent, Out) end)
    end,
    lists:foreach(SpawnWorker, lists:seq(1, NumSchedulers)),
    NumSchedulers.

worker_loop(Parent, Out) ->
    Options = [report_errors, report_warnings, debug_info, {outdir, Out}],
    erlang:send(Parent, {work_please, self()}),
    receive
        {module, Module} -> 
            log({compiling, Module}),
            case compile:file(Module, Options) of
                {ok, _} -> 
                    log({compiled, Module});
                error -> 
                    log({failed, Module}),
                    erlang:send(Parent, failed)
            end,
            worker_loop(Parent, Out)
    end.

add_lib_to_erlang_path(Lib) ->
    code:add_paths(filelib:wildcard([Lib, "/*/ebin"])) .

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

configure_logging() ->
    Enabled = os:getenv("GLEAM_LOG") /= false,
    persistent_term:put(gleam_logging_enabled, Enabled).

log(Term) ->
    case persistent_term:get(gleam_logging_enabled) of
        true -> erlang:display(Term), ok;
        false -> ok
    end.
