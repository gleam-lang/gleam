-record(heap, {
    root :: gleamy_structures@heap@pairing_heap:t(any()),
    compare :: fun((any(), any()) -> gleam@order:order())
}).
