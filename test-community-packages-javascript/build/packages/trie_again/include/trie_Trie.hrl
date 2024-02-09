-record(trie, {
    entry :: gleam@option:option(any()),
    children_map :: gleam@map:map_(any(), trie:trie(any(), any()))
}).
