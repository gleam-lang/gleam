#!/usr/bin/env erlang

% TODO: build in parallel
main([ProfilePath]) ->
    ErlangFiles = filelib:wildcard([ProfilePath, "/**/{src,test}/*.erl"]),
    lists:foreach(fun(F) -> compile(F) end, ErlangFiles).

compile(ErlangFile) ->
    EBin = ebin_path(ErlangFile),
    ok = filelib:ensure_dir([EBin, $/]),
    ErlangName = filename:rootname(ErlangFile),
    {ok, _} = compile:file(ErlangName, [{outdir, EBin}, return_errors]).

ebin_path(File) ->
    PackageRoot = filename:dirname(filename:dirname(File)),
    filename:join(PackageRoot, "ebin").
