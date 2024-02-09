-record(tree, {
    root :: gleamy_structures@tree@binary_search_tree:node_(any()),
    compare :: fun((any(), any()) -> gleam@order:order())
}).
