#!/usr/bin/env escript
-mode(compile).

% TODO: Don't concurrently print warnings and errors
% TODO: Some tests

main(_) -> compile_package_loop().

compile_package_loop() ->
    case file:read_line(standard_io) of
        eof -> ok;
        {ok, Line} ->
            Chars = unicode:characters_to_list(Line),
            {ok, Tokens, _} = erl_scan:string(Chars),
            {ok, {Lib, Out, Modules}} = erl_parse:parse_term(Tokens),
            case compile_package(Lib, Out, Modules) of
                ok -> io:put_chars("gleam-compile-result-ok\n");
                err -> io:put_chars("gleam-compile-result-error\n")
            end,
            compile_package_loop()
    end.

compile_package(Lib, Out, Modules) ->
    IsElixirModule = fun(Module) ->
        filename:extension(Module) =:= ".ex"
    end,
    {ElixirModules, ErlangModules} = lists:partition(IsElixirModule, Modules),
    ok = configure_logging(),
    ok = add_lib_to_erlang_path(Lib),
    ok = filelib:ensure_dir([Out, $/]),
    {ErlangOk, _ErlangBeams} = compile_erlang(ErlangModules, Out),
    {ElixirOk, _ElixirBeams} = case ErlangOk of
        true -> compile_elixir(ElixirModules, Out);
        false -> {false, []}
    end,
    ok = del_lib_from_erlang_path(Lib),
    case ErlangOk and ElixirOk of
        true -> ok;
        false -> err
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
    Error = [
        "The program elixir was not found. Is it installed?",
        $\n,
        "Documentation for installing Elixir can be viewed here:",
        $\n,
        "https://elixir-lang.org/install.html"
    ],
    case Modules of
        [] -> {true, []};
        _ ->
            log({starting, "compiler.app"}),
            ok = application:start(compiler),
            log({starting, "elixir.app"}),
            case application:start(elixir) of
                ok -> do_compile_elixir(Modules, Out);
                _ ->
                    io:put_chars(standard_error, [Error, $\n]),
                    {false, []}
            end
    end.

do_compile_elixir(Modules, Out) ->
    ModuleBins = lists:map(fun(Module) ->
        log({compiling, Module}),
        list_to_binary(Module)
    end, Modules),
    OutBin = list_to_binary(Out),
    Options = [{dest, OutBin}],
    % Silence "redefining module" warnings.
    % Compiled modules in the build directory are added to the code path.
    % These warnings result from recompiling loaded modules.
    % TODO: This line can likely be removed if/when the build directory is cleaned before every compilation.
    'Elixir.Code':compiler_options([{ignore_module_conflict, true}]),
    case 'Elixir.Kernel.ParallelCompiler':compile_to_path(ModuleBins, OutBin, Options) of
        {ok, ModuleAtoms, _} ->
            ToBeam = fun(ModuleAtom) ->
                Beam = filename:join(Out, atom_to_list(ModuleAtom)) ++ ".beam",
                log({compiled, Beam}),
                Beam
            end,
            {true, lists:map(ToBeam, ModuleAtoms)};
        {error, Errors, _} ->
            % Log all filenames associated with modules that failed to compile.
            % Note: The compiler prints compilation errors upon encountering them.
            ErrorFiles = lists:usort([File || {File, _, _} <- Errors]),
            Log = fun(File) ->
                log({failed, binary_to_list(File)})
            end,
            lists:foreach(Log, ErrorFiles),
            {false, []};
        _ -> {false, []}
    end.

add_lib_to_erlang_path(Lib) ->
    code:add_paths(filelib:wildcard([Lib, "/*/ebin"])).

del_lib_from_erlang_path(Lib) ->
    code:del_paths(filelib:wildcard([Lib, "/*/ebin"])).

configure_logging() ->
    Enabled = os:getenv("GLEAM_LOG") /= false,
    persistent_term:put(gleam_logging_enabled, Enabled).

log(Term) ->
    case persistent_term:get(gleam_logging_enabled) of
        true -> erlang:display(Term), ok;
        false -> ok
    end.
