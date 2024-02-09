-record(heap, {
    root :: gleamy_structures@heap@leftist_heap:t(any()),
    compare :: fun((any(), any()) -> gleam@order:order())
}).
