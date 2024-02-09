-module(mist@internal@logger).
-compile([no_auto_import, nowarn_unused_vars]).

-export([error/1]).

-spec error(any()) -> nil.
error(Data) ->
    logger:error(unicode:characters_to_list(<<"~tp"/utf8>>), [Data]).
