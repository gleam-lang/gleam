-record(tree, {
    root :: gleamy_structures@tree@red_black_tree:node_(any()),
    compare :: fun((any(), any()) -> gleam@order:order())
}).
