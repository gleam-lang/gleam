#!/usr/bin/env escript

% TODO: Don't concurrently print warnings and errors
% TODO: Some tests

-record(arguments, {lib = "./", out = "./", modules = []}).

main(Args) ->
    #arguments{out = Out, lib = Lib, modules = Modules} = parse(Args),
    IsElixirModule = fun(Module) ->
        filename:extension(Module) =:= ".ex"
    end,
    {ElixirModules, ErlangModules} = lists:partition(IsElixirModule, Modules),
    ok = configure_logging(),
    ok = add_lib_to_erlang_path(Lib),
    ok = filelib:ensure_dir([Out, $/]),
    {ErlangOk, ErlangBeams} = compile_erlang(ErlangModules, Out),
    {ElixirOk, ElixirBeams} = compile_elixir(ElixirModules, Out),
    JournalOk = write_journal(ErlangBeams ++ ElixirBeams, Lib),
    case ErlangOk and ElixirOk and JournalOk of
        true -> ok;
        false -> erlang:halt(1)
    end.

compile_erlang(Modules, Out) ->
    Workers = start_compiler_workers(Out),
    ok = producer_loop(Modules, Workers),
    collect_results({true, []}).

collect_results(Acc = {Result, Beams}) ->
    receive
        {compiled, Beam} -> collect_results({Result, [Beam | Beams]});
        failed -> collect_results({false, Beams})
        after 0 -> Acc
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
                {ok, ModuleName} ->
                    Beam = filename:join(Out, ModuleName) ++ ".beam",
                    Message = {compiled, Beam},
                    log(Message),
                    erlang:send(Parent, Message);
                error ->
                    log({failed, Module}),
                    erlang:send(Parent, failed)
            end,
            worker_loop(Parent, Out)
    end.

compile_elixir(Modules, Out) ->
    case Modules of
        [] -> {true, []};
        _ -> do_compile_elixir(Modules, Out)
    end.

do_compile_elixir(Modules, Out) ->
    ModuleBins = lists:map(fun(Module) ->
        log({compiling, Module}),
        list_to_binary(Module)
    end, Modules),
    OutBin = list_to_binary(Out),
    Options = [{dest, OutBin}],
    ShouldLog = should_log(),
    ok = application:start(compiler),
    ok = application:start(elixir),
    'Elixir.Code':compiler_options([{ignore_module_conflict, true}]),
    case 'Elixir.Kernel.ParallelCompiler':compile_to_path(ModuleBins, OutBin, Options) of
        {ok, ModuleAtoms, _} ->
            ToBeam = fun(ModuleAtom) ->
                Beam = filename:join(Out, atom_to_list(ModuleAtom)) ++ ".beam",
                log({compiled, Beam}),
                Beam
            end,
            {true, lists:map(ToBeam, ModuleAtoms)};
        {error, Errors, _} when ShouldLog ->
            ErrorFiles = lists:usort([File || {File, _, _} <- Errors]),
            Log = fun(File) ->
                log({failed, binary_to_list(File)})
            end,
            lists:foreach(Log, ErrorFiles),
            {false, []};
        _ -> {false, []}
    end.

write_journal(Beams, Lib) ->
    NewLine = io_lib:nl(),
    Contents = [string:join(Beams, NewLine), NewLine],
    Journal = filename:join(Lib, "gleam_build_journal.tmp"),
    case file:write_file(Journal, Contents) of
        ok ->
            log({wrote, Journal}),
            true;
        {error, Reason} ->
            log({failed_to_write, Journal, Reason}),
            false
    end.

add_lib_to_erlang_path(Lib) ->
    code:add_paths(filelib:wildcard([Lib, "/*/ebin"])).

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
    case should_log() of
        true -> erlang:display(Term), ok;
        false -> ok
    end.

should_log() ->
    persistent_term:get(gleam_logging_enabled).
