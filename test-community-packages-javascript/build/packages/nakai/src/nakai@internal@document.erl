-module(nakai@internal@document).
-compile([no_auto_import, nowarn_unused_vars]).

-export([new/0, merge/2, concat/1, from_doctype/1, append_html_attrs/2, append_body_attrs/2, from_head/1, append_head/2, from_body/1, append_body/2, replace_body/2, from_script/1, into_head/1]).
-export_type([document/0]).

-type document() :: {document,
        gleam@option:option(binary()),
        gleam@string_builder:string_builder(),
        gleam@string_builder:string_builder(),
        gleam@string_builder:string_builder(),
        gleam@string_builder:string_builder(),
        list(binary())}.

-spec new() -> document().
new() ->
    {document,
        none,
        gleam@string_builder:new(),
        gleam@string_builder:new(),
        gleam@string_builder:new(),
        gleam@string_builder:new(),
        []}.

-spec merge(document(), document()) -> document().
merge(Self, New) ->
    {document,
        gleam@option:'or'(erlang:element(2, New), erlang:element(2, Self)),
        gleam@string_builder:append_builder(
            erlang:element(3, Self),
            erlang:element(3, New)
        ),
        gleam@string_builder:append_builder(
            erlang:element(4, Self),
            erlang:element(4, New)
        ),
        gleam@string_builder:append_builder(
            erlang:element(5, Self),
            erlang:element(5, New)
        ),
        gleam@string_builder:append_builder(
            erlang:element(6, Self),
            erlang:element(6, New)
        ),
        gleam@list:append(erlang:element(7, Self), erlang:element(7, New))}.

-spec concat(list(document())) -> document().
concat(Docs) ->
    _pipe = Docs,
    gleam@list:fold(_pipe, new(), fun merge/2).

-spec from_doctype(binary()) -> document().
from_doctype(Doctype) ->
    erlang:setelement(2, new(), {some, Doctype}).

-spec append_html_attrs(document(), gleam@string_builder:string_builder()) -> document().
append_html_attrs(Self, Html_attrs) ->
    erlang:setelement(
        3,
        Self,
        gleam@string_builder:append_builder(erlang:element(3, Self), Html_attrs)
    ).

-spec append_body_attrs(document(), gleam@string_builder:string_builder()) -> document().
append_body_attrs(Self, Body_attrs) ->
    erlang:setelement(
        4,
        Self,
        gleam@string_builder:append_builder(erlang:element(4, Self), Body_attrs)
    ).

-spec from_head(gleam@string_builder:string_builder()) -> document().
from_head(Head) ->
    erlang:setelement(5, new(), Head).

-spec append_head(document(), gleam@string_builder:string_builder()) -> document().
append_head(Self, Head) ->
    erlang:setelement(
        5,
        Self,
        gleam@string_builder:append_builder(erlang:element(5, Self), Head)
    ).

-spec from_body(gleam@string_builder:string_builder()) -> document().
from_body(Body) ->
    erlang:setelement(6, new(), Body).

-spec append_body(document(), gleam@string_builder:string_builder()) -> document().
append_body(Self, Body) ->
    erlang:setelement(
        6,
        Self,
        gleam@string_builder:append_builder(erlang:element(6, Self), Body)
    ).

-spec replace_body(document(), gleam@string_builder:string_builder()) -> document().
replace_body(Self, Body) ->
    erlang:setelement(6, Self, Body).

-spec from_script(binary()) -> document().
from_script(Script) ->
    erlang:setelement(7, new(), [Script]).

-spec into_head(document()) -> document().
into_head(State) ->
    erlang:setelement(
        6,
        erlang:setelement(
            5,
            State,
            gleam@string_builder:append_builder(
                erlang:element(5, State),
                erlang:element(6, State)
            )
        ),
        gleam@string_builder:new()
    ).
