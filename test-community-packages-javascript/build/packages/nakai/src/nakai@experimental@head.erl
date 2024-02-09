-module(nakai@experimental@head).
-compile([no_auto_import, nowarn_unused_vars]).

-export([title/1, link/2, meta/2, http_equiv/2, charset/1]).

-spec title(binary()) -> nakai@html:node_(any()).
title(Title) ->
    {head, [nakai@html:title(Title)]}.

-spec link(binary(), binary()) -> nakai@html:node_(any()).
link(Rel, Href) ->
    {head,
        [nakai@html:link(
                [nakai@html@attrs:rel(Rel), nakai@html@attrs:href(Href)]
            )]}.

-spec meta(binary(), binary()) -> nakai@html:node_(any()).
meta(Name, Content) ->
    {head,
        [nakai@html:meta(
                [nakai@html@attrs:name(Name), nakai@html@attrs:content(Content)]
            )]}.

-spec http_equiv(binary(), binary()) -> nakai@html:node_(any()).
http_equiv(Header, Content) ->
    {head,
        [nakai@html:meta(
                [nakai@html@attrs:http_equiv(Header),
                    nakai@html@attrs:content(Content)]
            )]}.

-spec charset(binary()) -> nakai@html:node_(any()).
charset(Charset) ->
    {head, [nakai@html:meta([nakai@html@attrs:charset(Charset)])]}.
