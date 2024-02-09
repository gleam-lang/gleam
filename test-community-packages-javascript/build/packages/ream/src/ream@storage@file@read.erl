-module(ream@storage@file@read).
-compile([no_auto_import, nowarn_unused_vars]).

-export_type([result/0]).

-type result() :: {ok, bitstring()} | eof | {error, gleam@erlang@file:reason()}.


