-module(ream@storage@file@write).
-compile([no_auto_import, nowarn_unused_vars]).

-export_type([result/0]).

-type result() :: ok | {error, gleam@erlang@file:reason()}.


