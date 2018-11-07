-module(rebar_compiler_gleam).

-behaviour(rebar_compiler).

-export([context/1,
         needed_files/3,
         dependencies/3,
         compile/4,
         clean/2]).

% -include("rebar.hrl").
% -include_lib("stdlib/include/erl_compile.hrl").

context(AppInfo) ->
    Dir = rebar_app_info:dir(AppInfo),
    Mappings = [{".bin", filename:join([Dir, "priv", "mibs"])},
               {".hrl", filename:join(Dir, "include")}],

    #{src_dirs => ["mibs"],
      include_dirs => [],
      src_ext => ".mib",
      out_mappings => Mappings}.

needed_files(_, FoundFiles, AppInfo) ->
    FirstFiles = [],

    %% Remove first files from found files
    RestFiles = [Source || Source <- FoundFiles, not lists:member(Source, FirstFiles)],

    Opts = rebar_opts:get(rebar_app_info:opts(AppInfo), mib_opts, []),
    {{FirstFiles, Opts}, {RestFiles, Opts}}.

dependencies(_, _, _) ->
    [].

compile(Source, OutDirs, _, Opts) ->
    {_, BinOut} = lists:keyfind(".bin", 1, OutDirs),
    {_, HrlOut} = lists:keyfind(".hrl", 1, OutDirs),

    ok = rebar_file_utils:ensure_dir(BinOut),
    ok = rebar_file_utils:ensure_dir(HrlOut),
    Mib = filename:join(BinOut, filename:basename(Source, ".mib")),
    HrlFilename = Mib ++ ".hrl",

    AllOpts = [{outdir, BinOut}, {i, [BinOut]}] ++ Opts,

    case snmpc:compile(Source, AllOpts) of
        {ok, _} ->
            % MibToHrlOpts =
            %     case proplists:get_value(verbosity, AllOpts, undefined) of
            %         undefined ->
            %             #options{specific = [],
            %                      cwd = rebar_dir:get_cwd()};
            %         Verbosity ->
            %             #options{specific = [{verbosity, Verbosity}],
            %                      cwd = rebar_dir:get_cwd()}
            %     end,
            % ok = snmpc:mib_to_hrl(Mib, Mib, MibToHrlOpts),
            % rebar_file_utils:mv(HrlFilename, HrlOut),
            ok;
        {error, compilation_failed} ->
            rebar_utils:abort()
    end.

clean(MibFiles, AppInfo) ->
    AppDir = rebar_app_info:dir(AppInfo),
    MIBs = [filename:rootname(filename:basename(MIB)) || MIB <- MibFiles],
    rebar_file_utils:delete_each(
      [filename:join([AppDir, "include", MIB++".hrl"]) || MIB <- MIBs]),
    ok = rebar_file_utils:rm_rf(filename:join([AppDir, "priv/mibs/*.bin"])).
