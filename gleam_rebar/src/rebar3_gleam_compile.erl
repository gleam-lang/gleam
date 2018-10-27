-module(rebar3_gleam_compile).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).
% for rebar3_gleam_ct
-export([do/3]).

-include_lib("kernel/include/file.hrl").

-define(PROVIDER, compile).
-define(DEPS, [{default, install_deps}, {default, app_discovery}]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},          % The 'user friendly' name of the task
            {module, ?MODULE},          % The module implementation of the task
            {namespace, gleam},
            {bare, false},
            {deps, ?DEPS},              % The list of dependencies
            {example, "rebar gleam compile"}, % How to use the plugin
            % list of options understood by the plugin
            {opts, [{format, undefined, "format", string, help(format)},
                    {file, undefined, "file", string, help(file)}]},
            {short_desc, "gleam rebar3 plugin"},
            {desc, ""}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    do(State, "src", fun rebar_app_info:ebin_dir/1).

do(State, SourceDirName, GetTargetDirFn) ->
    DepsPaths = rebar_state:code_paths(State, all_deps),
    %PluginDepsPaths = rebar_state:code_paths(State, all_plugin_deps),
    %rebar_utils:remove_from_code_path(PluginDepsPaths),
    code:add_pathsa(DepsPaths),

    {RawOpts, _} = rebar_state:command_parsed_args(State) ,
    Format = proplists:get_value(format, RawOpts, "beam"),
    case proplists:get_value(file, RawOpts, undefined) of
        undefined -> compile_all(State, Format, SourceDirName, GetTargetDirFn);
        FilePath -> compile_file(State, FilePath, Format, GetTargetDirFn)
    end.

compile_file(State, Path, Format, GetTargetDirFn) ->
    AppInfo = case rebar_state:current_app(State) of
               undefined -> hd(rebar_state:project_apps(State));
               AppInfo0 -> AppInfo0
           end,
    TargetDir = GetTargetDirFn(AppInfo),
    ErlOpts = rebar_state:get(State, erl_opts, []),
    rebar_api:info("Compiling ~s", [Path]),
    compile(Format, Path, TargetDir, ErlOpts),
    {ok, State}.

compile_all(State, Format, SourceDirName, GetTargetDirFn) ->
    Apps = case rebar_state:current_app(State) of
               undefined -> rebar_state:project_apps(State);
               AppInfo -> [AppInfo]
           end,

    FirstFiles = [],
    SourceExt = ".gleam",
    TargetExt = "." ++ Format,

    [begin
         Opts = rebar_app_info:opts(AppInfo),
         TargetDir = GetTargetDirFn(AppInfo),
         SourceDir = filename:join(rebar_app_info:dir(AppInfo), SourceDirName),

         CompileFun = fun(Source, Target, Opts1) ->
                              ErlOpts = rebar_opts:erl_opts(Opts1),
                              compile_source(ErlOpts, Source, Target, Format)
                      end,

         rebar_base_compiler:run(Opts, FirstFiles, SourceDir, SourceExt,
                                 TargetDir, TargetExt, CompileFun, [])
     end || AppInfo <- Apps],

    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Private API
%% ===================================================================

compile("beam", Path, DestPath, Options) ->
    {ok, Source} = file:read_file(Path),
    ModName = filename:basename(Path, ".gleam"),
    ListSource = binary_to_list(Source),
    BeamFileName = "gleam_" ++ filename:basename(filename:rootname(Path)) ++ ".beam",
    BeamPath = filename:join(filename:dirname(DestPath), BeamFileName),
    Beam = gleam_compiler:source_to_binary(ListSource, ModName),
    ok = file:write_file(BeamPath, Beam);

compile(Format, _Path, _DestPath, _ErlOpts) ->
    rebar_api:error("Invalid format: ~s", [Format]).

compile_source(ErlOpts, Source, DestPath, Format) ->
    {ok, SourceFileInfo} = file:read_file_info(Source),
    NeedsCompile = case file:read_file_info(DestPath) of
                       {ok, DestFileInfo} ->
                           #file_info{mtime=SourceMTime}=SourceFileInfo,
                           #file_info{mtime=DestMTime}=DestFileInfo,
                           SourceMSecs = calendar:datetime_to_gregorian_seconds(SourceMTime),
                           DestMSecs = calendar:datetime_to_gregorian_seconds(DestMTime),
                           SourceMSecs =/= DestMSecs;
                       {error, _} ->
                           true
                   end,
    ok = filelib:ensure_dir(DestPath),
    if NeedsCompile ->
            rebar_api:info("Compiling ~s", [Source]),
            compile(Format, Source, filename:dirname(DestPath), ErlOpts);
       true ->
            rebar_api:info("Skipping  ~s", [Source])
    end.

help(format) -> "format to compile code to, beam";
help(file) -> "file to compile, if omited all files in the project are compiled".
