-record(tree, {
    root :: gleamy_structures@tree@red_black_tree_kv:node_(any(), any()),
    compare :: fun((any(), any()) -> gleam@order:order())
}).
