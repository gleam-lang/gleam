-module(nakai@internal@render).
-compile([no_auto_import, nowarn_unused_vars]).

-export([render_document/1, render_inline/1]).
-export_type([builder/2]).

-type builder(HVS, HVT) :: {builder,
        fun((nakai@html:node_(HVS)) -> HVT),
        fun((list(HVT)) -> HVT)}.

-spec render_doctype(binary()) -> gleam@string_builder:string_builder().
render_doctype(Doctype) ->
    gleam@string_builder:from_strings(
        [<<"<!DOCTYPE "/utf8>>, Doctype, <<">\n"/utf8>>]
    ).

-spec render_children(list(nakai@html:node_(HVU)), builder(HVU, HVX)) -> HVX.
render_children(Children, Builder) ->
    _pipe = Children,
    _pipe@1 = gleam@list:map(_pipe, erlang:element(2, Builder)),
    (erlang:element(3, Builder))(_pipe@1).

-spec render_attr(nakai@html@attrs:attr(any())) -> gleam@string_builder:string_builder().
render_attr(Attr) ->
    case Attr of
        {attr, Name, Value} ->
            Sanitized_value = begin
                _pipe = Value,
                _pipe@1 = gleam@string:replace(
                    _pipe,
                    <<"\""/utf8>>,
                    <<"&quot;"/utf8>>
                ),
                gleam@string:replace(_pipe@1, <<">"/utf8>>, <<"&gt;"/utf8>>)
            end,
            gleam@string_builder:from_strings(
                [<<" "/utf8>>,
                    Name,
                    <<"=\""/utf8>>,
                    Sanitized_value,
                    <<"\""/utf8>>]
            );

        {event, _, _} ->
            gleam@string_builder:new()
    end.

-spec render_attrs(list(nakai@html@attrs:attr(any()))) -> gleam@string_builder:string_builder().
render_attrs(Attrs) ->
    _pipe = Attrs,
    _pipe@1 = gleam@list:map(_pipe, fun render_attr/1),
    gleam@list:fold(
        _pipe@1,
        gleam@string_builder:new(),
        fun gleam@string_builder:append_builder/2
    ).

-spec render_document_node(nakai@html:node_(any())) -> nakai@internal@document:document().
render_document_node(Tree) ->
    case Tree of
        {doctype, Doctype} ->
            nakai@internal@document:from_doctype(Doctype);

        {html, Attrs, Children} ->
            _pipe = render_children(
                Children,
                {builder,
                    fun render_document_node/1,
                    fun nakai@internal@document:concat/1}
            ),
            nakai@internal@document:append_html_attrs(
                _pipe,
                render_attrs(Attrs)
            );

        {head, Children@1} ->
            _pipe@1 = render_children(
                Children@1,
                {builder,
                    fun render_document_node/1,
                    fun nakai@internal@document:concat/1}
            ),
            nakai@internal@document:into_head(_pipe@1);

        {body, Attrs@1, Children@2} ->
            _pipe@2 = render_children(
                Children@2,
                {builder,
                    fun render_document_node/1,
                    fun nakai@internal@document:concat/1}
            ),
            nakai@internal@document:append_body_attrs(
                _pipe@2,
                render_attrs(Attrs@1)
            );

        {fragment, Children@3} ->
            render_children(
                Children@3,
                {builder,
                    fun render_document_node/1,
                    fun nakai@internal@document:concat/1}
            );

        {element, Tag, Attrs@2, Children@4} ->
            Child_document = render_children(
                Children@4,
                {builder,
                    fun render_document_node/1,
                    fun nakai@internal@document:concat/1}
            ),
            _pipe@3 = gleam@string_builder:concat(
                [gleam@string_builder:from_strings([<<"<"/utf8>>, Tag]),
                    render_attrs(Attrs@2),
                    gleam@string_builder:from_string(<<">"/utf8>>),
                    erlang:element(6, Child_document),
                    gleam@string_builder:from_strings(
                        [<<"</"/utf8>>, Tag, <<">"/utf8>>]
                    )]
            ),
            nakai@internal@document:replace_body(Child_document, _pipe@3);

        {leaf_element, Tag@1, Attrs@3} ->
            _pipe@4 = gleam@string_builder:concat(
                [gleam@string_builder:from_strings([<<"<"/utf8>>, Tag@1]),
                    render_attrs(Attrs@3),
                    gleam@string_builder:from_string(<<" />"/utf8>>)]
            ),
            nakai@internal@document:from_body(_pipe@4);

        {comment, Content} ->
            Content@1 = begin
                _pipe@5 = Content,
                gleam@string:replace(_pipe@5, <<"-->"/utf8>>, <<""/utf8>>)
            end,
            _pipe@6 = gleam@string_builder:from_strings(
                [<<"<!-- "/utf8>>, Content@1, <<" -->"/utf8>>]
            ),
            nakai@internal@document:from_body(_pipe@6);

        {text, Content@2} ->
            _pipe@7 = gleam@string_builder:from_string(Content@2),
            _pipe@8 = gleam@string_builder:replace(
                _pipe@7,
                <<"&"/utf8>>,
                <<"&amp;"/utf8>>
            ),
            _pipe@9 = gleam@string_builder:replace(
                _pipe@8,
                <<"<"/utf8>>,
                <<"&lt;"/utf8>>
            ),
            _pipe@10 = gleam@string_builder:replace(
                _pipe@9,
                <<">"/utf8>>,
                <<"&gt;"/utf8>>
            ),
            nakai@internal@document:from_body(_pipe@10);

        {unsafe_text, Content@3} ->
            _pipe@11 = gleam@string_builder:from_string(Content@3),
            nakai@internal@document:from_body(_pipe@11);

        {script, Script} ->
            nakai@internal@document:from_script(Script);

        nothing ->
            nakai@internal@document:new()
    end.

-spec render_script(binary()) -> gleam@string_builder:string_builder().
render_script(Script) ->
    gleam@string_builder:concat(
        [gleam@string_builder:from_string(<<"<script>"/utf8>>),
            gleam@string_builder:from_string(Script),
            gleam@string_builder:from_string(<<"</script>\n"/utf8>>)]
    ).

-spec render_scripts(list(binary())) -> gleam@string_builder:string_builder().
render_scripts(Scripts) ->
    _pipe = Scripts,
    _pipe@1 = gleam@list:map(_pipe, fun render_script/1),
    gleam@string_builder:concat(_pipe@1).

-spec render_document(nakai@html:node_(any())) -> gleam@string_builder:string_builder().
render_document(Tree) ->
    Result = render_document_node(Tree),
    gleam@string_builder:concat(
        [render_doctype(
                begin
                    _pipe = erlang:element(2, Result),
                    gleam@option:unwrap(_pipe, <<"html"/utf8>>)
                end
            ),
            gleam@string_builder:from_string(<<"<html"/utf8>>),
            erlang:element(3, Result),
            gleam@string_builder:from_string(
                <<">\n<head>"/utf8,
                    (<<"
<meta charset=\"utf-8\" />
<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" />
"/utf8>>)/binary>>
            ),
            erlang:element(5, Result),
            gleam@string_builder:from_string(<<"</head>\n<body"/utf8>>),
            erlang:element(4, Result),
            gleam@string_builder:from_string(<<">"/utf8>>),
            erlang:element(6, Result),
            render_scripts(erlang:element(7, Result)),
            gleam@string_builder:from_string(<<"</body>\n</html>\n"/utf8>>)]
    ).

-spec render_inline_node(nakai@html:node_(any())) -> gleam@string_builder:string_builder().
render_inline_node(Tree) ->
    case Tree of
        {doctype, Doctype} ->
            render_doctype(Doctype);

        {html, Attrs, Children} ->
            render_inline_node({element, <<"html"/utf8>>, Attrs, Children});

        {head, Children@1} ->
            render_inline_node({element, <<"head"/utf8>>, [], Children@1});

        {body, Attrs@1, Children@2} ->
            render_inline_node({element, <<"body"/utf8>>, Attrs@1, Children@2});

        {fragment, Children@3} ->
            render_children(
                Children@3,
                {builder,
                    fun render_inline_node/1,
                    fun gleam@string_builder:concat/1}
            );

        {element, Tag, Attrs@2, Children@4} ->
            Child_document = render_children(
                Children@4,
                {builder,
                    fun render_inline_node/1,
                    fun gleam@string_builder:concat/1}
            ),
            gleam@string_builder:concat(
                [gleam@string_builder:from_strings([<<"<"/utf8>>, Tag]),
                    render_attrs(Attrs@2),
                    gleam@string_builder:from_string(<<">"/utf8>>),
                    Child_document,
                    gleam@string_builder:from_strings(
                        [<<"</"/utf8>>, Tag, <<">"/utf8>>]
                    )]
            );

        {leaf_element, Tag@1, Attrs@3} ->
            gleam@string_builder:concat(
                [gleam@string_builder:from_strings([<<"<"/utf8>>, Tag@1]),
                    render_attrs(Attrs@3),
                    gleam@string_builder:from_string(<<" />"/utf8>>)]
            );

        {comment, Content} ->
            Content@1 = begin
                _pipe = Content,
                gleam@string:replace(_pipe, <<"-->"/utf8>>, <<""/utf8>>)
            end,
            gleam@string_builder:from_strings(
                [<<"<!-- "/utf8>>, Content@1, <<" -->"/utf8>>]
            );

        {text, Content@2} ->
            _pipe@1 = gleam@string_builder:from_string(Content@2),
            _pipe@2 = gleam@string_builder:replace(
                _pipe@1,
                <<"&"/utf8>>,
                <<"&amp;"/utf8>>
            ),
            _pipe@3 = gleam@string_builder:replace(
                _pipe@2,
                <<"<"/utf8>>,
                <<"&lt;"/utf8>>
            ),
            gleam@string_builder:replace(_pipe@3, <<">"/utf8>>, <<"&gt;"/utf8>>);

        {unsafe_text, Content@3} ->
            gleam@string_builder:from_string(Content@3);

        {script, Script} ->
            render_inline_node(
                {element, <<"script"/utf8>>, [], [{text, Script}]}
            );

        nothing ->
            gleam@string_builder:new()
    end.

-spec render_inline(nakai@html:node_(any())) -> gleam@string_builder:string_builder().
render_inline(Tree) ->
    render_inline_node(Tree).
