-module(nakai@experimental@web_components).
-compile([no_auto_import, nowarn_unused_vars]).

-export([slot/1, template/2]).

-spec slot(list(nakai@html@attrs:attr(ICF))) -> nakai@html:node_(ICF).
slot(Attrs) ->
    {leaf_element, <<"slot"/utf8>>, Attrs}.

-spec template(list(nakai@html@attrs:attr(ICJ)), list(nakai@html:node_(ICJ))) -> nakai@html:node_(ICJ).
template(Attrs, Children) ->
    {element, <<"template"/utf8>>, Attrs, Children}.
