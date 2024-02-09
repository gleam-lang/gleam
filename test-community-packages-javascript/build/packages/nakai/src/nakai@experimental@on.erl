-module(nakai@experimental@on).
-compile([no_auto_import, nowarn_unused_vars]).

-export([click/1]).

-spec click(binary()) -> nakai@html@attrs:attr(any()).
click(Script) ->
    {attr, <<"onclick"/utf8>>, Script}.
