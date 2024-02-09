-module(nakai).
-compile([no_auto_import, nowarn_unused_vars]).

-export([to_string_builder/1, to_string/1, to_inline_string_builder/1, to_inline_string/1]).

-spec to_string_builder(nakai@html:node_(any())) -> gleam@string_builder:string_builder().
to_string_builder(Tree) ->
    nakai@internal@render:render_document(Tree).

-spec to_string(nakai@html:node_(any())) -> binary().
to_string(Tree) ->
    _pipe = nakai@internal@render:render_document(Tree),
    gleam@string_builder:to_string(_pipe).

-spec to_inline_string_builder(nakai@html:node_(any())) -> gleam@string_builder:string_builder().
to_inline_string_builder(Tree) ->
    nakai@internal@render:render_inline(Tree).

-spec to_inline_string(nakai@html:node_(any())) -> binary().
to_inline_string(Tree) ->
    _pipe = nakai@internal@render:render_inline(Tree),
    gleam@string_builder:to_string(_pipe).
