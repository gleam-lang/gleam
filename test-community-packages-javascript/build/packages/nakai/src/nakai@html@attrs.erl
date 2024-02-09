-module(nakai@html@attrs).
-compile([no_auto_import, nowarn_unused_vars]).

-export([data/2, accept/1, accept_charset/1, action/1, alt/1, async/0, autocapitalize/1, autocomplete/1, autofocus/0, autoplay/0, capture/1, charset/1, checked/0, cite/1, class/1, content/1, contenteditable/0, crossorigin/0, defer/0, disabled/0, draggable/0, for/1, formaction/1, height/1, href/1, http_equiv/1, id/1, integrity/1, lang/1, loop/0, method/1, name/1, placeholder/1, preload/0, property/1, readonly/0, rel/1, selected/0, src/1, style/1, tabindex/1, target/1, title/1, type_/1, value/1, width/1]).
-export_type([attr/1]).

-type attr(FNI) :: {attr, binary(), binary()} | {event, binary(), FNI}.

-spec data(binary(), binary()) -> attr(any()).
data(Name, Value) ->
    {attr, <<"data-"/utf8, Name/binary>>, Value}.

-spec accept(binary()) -> attr(any()).
accept(Value) ->
    {attr, <<"accept"/utf8>>, Value}.

-spec accept_charset(binary()) -> attr(any()).
accept_charset(Value) ->
    {attr, <<"accept-charset"/utf8>>, Value}.

-spec action(binary()) -> attr(any()).
action(Value) ->
    {attr, <<"action"/utf8>>, Value}.

-spec alt(binary()) -> attr(any()).
alt(Value) ->
    {attr, <<"alt"/utf8>>, Value}.

-spec async() -> attr(any()).
async() ->
    {attr, <<"async"/utf8>>, <<"true"/utf8>>}.

-spec autocapitalize(binary()) -> attr(any()).
autocapitalize(Value) ->
    {attr, <<"autocapitalize"/utf8>>, Value}.

-spec autocomplete(binary()) -> attr(any()).
autocomplete(Value) ->
    {attr, <<"autocomplete"/utf8>>, Value}.

-spec autofocus() -> attr(any()).
autofocus() ->
    {attr, <<"autofocus"/utf8>>, <<"true"/utf8>>}.

-spec autoplay() -> attr(any()).
autoplay() ->
    {attr, <<"autoplay"/utf8>>, <<"true"/utf8>>}.

-spec capture(binary()) -> attr(any()).
capture(Value) ->
    {attr, <<"capture"/utf8>>, Value}.

-spec charset(binary()) -> attr(any()).
charset(Value) ->
    {attr, <<"charset"/utf8>>, Value}.

-spec checked() -> attr(any()).
checked() ->
    {attr, <<"checked"/utf8>>, <<"true"/utf8>>}.

-spec cite(binary()) -> attr(any()).
cite(Value) ->
    {attr, <<"cite"/utf8>>, Value}.

-spec class(binary()) -> attr(any()).
class(Value) ->
    {attr, <<"class"/utf8>>, Value}.

-spec content(binary()) -> attr(any()).
content(Value) ->
    {attr, <<"content"/utf8>>, Value}.

-spec contenteditable() -> attr(any()).
contenteditable() ->
    {attr, <<"contenteditable"/utf8>>, <<"true"/utf8>>}.

-spec crossorigin() -> attr(any()).
crossorigin() ->
    {attr, <<"crossorigin"/utf8>>, <<"true"/utf8>>}.

-spec defer() -> attr(any()).
defer() ->
    {attr, <<"defer"/utf8>>, <<"true"/utf8>>}.

-spec disabled() -> attr(any()).
disabled() ->
    {attr, <<"disabled"/utf8>>, <<"true"/utf8>>}.

-spec draggable() -> attr(any()).
draggable() ->
    {attr, <<"draggable"/utf8>>, <<"true"/utf8>>}.

-spec for(binary()) -> attr(any()).
for(Value) ->
    {attr, <<"for"/utf8>>, Value}.

-spec formaction(binary()) -> attr(any()).
formaction(Value) ->
    {attr, <<"formaction"/utf8>>, Value}.

-spec height(binary()) -> attr(any()).
height(Value) ->
    {attr, <<"height"/utf8>>, Value}.

-spec href(binary()) -> attr(any()).
href(Value) ->
    {attr, <<"href"/utf8>>, Value}.

-spec http_equiv(binary()) -> attr(any()).
http_equiv(Value) ->
    {attr, <<"http-equiv"/utf8>>, Value}.

-spec id(binary()) -> attr(any()).
id(Value) ->
    {attr, <<"id"/utf8>>, Value}.

-spec integrity(binary()) -> attr(any()).
integrity(Value) ->
    {attr, <<"integrity"/utf8>>, Value}.

-spec lang(binary()) -> attr(any()).
lang(Value) ->
    {attr, <<"lang"/utf8>>, Value}.

-spec loop() -> attr(any()).
loop() ->
    {attr, <<"loop"/utf8>>, <<"true"/utf8>>}.

-spec method(binary()) -> attr(any()).
method(Value) ->
    {attr, <<"method"/utf8>>, Value}.

-spec name(binary()) -> attr(any()).
name(Value) ->
    {attr, <<"name"/utf8>>, Value}.

-spec placeholder(binary()) -> attr(any()).
placeholder(Value) ->
    {attr, <<"placeholder"/utf8>>, Value}.

-spec preload() -> attr(any()).
preload() ->
    {attr, <<"preload"/utf8>>, <<"true"/utf8>>}.

-spec property(binary()) -> attr(any()).
property(Value) ->
    {attr, <<"property"/utf8>>, Value}.

-spec readonly() -> attr(any()).
readonly() ->
    {attr, <<"readonly"/utf8>>, <<"true"/utf8>>}.

-spec rel(binary()) -> attr(any()).
rel(Value) ->
    {attr, <<"rel"/utf8>>, Value}.

-spec selected() -> attr(any()).
selected() ->
    {attr, <<"selected"/utf8>>, <<"true"/utf8>>}.

-spec src(binary()) -> attr(any()).
src(Value) ->
    {attr, <<"src"/utf8>>, Value}.

-spec style(binary()) -> attr(any()).
style(Value) ->
    {attr, <<"style"/utf8>>, Value}.

-spec tabindex(binary()) -> attr(any()).
tabindex(Value) ->
    {attr, <<"tabindex"/utf8>>, Value}.

-spec target(binary()) -> attr(any()).
target(Value) ->
    {attr, <<"target"/utf8>>, Value}.

-spec title(binary()) -> attr(any()).
title(Value) ->
    {attr, <<"title"/utf8>>, Value}.

-spec type_(binary()) -> attr(any()).
type_(Value) ->
    {attr, <<"type"/utf8>>, Value}.

-spec value(binary()) -> attr(any()).
value(Value) ->
    {attr, <<"value"/utf8>>, Value}.

-spec width(binary()) -> attr(any()).
width(Value) ->
    {attr, <<"width"/utf8>>, Value}.
