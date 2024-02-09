-module(glenvy@error).
-compile([no_auto_import, nowarn_unused_vars]).

-export_type([error/0]).

-type error() :: {io, binary()}.


