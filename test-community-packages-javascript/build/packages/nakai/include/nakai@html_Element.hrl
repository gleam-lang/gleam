-record(element, {
    tag :: binary(),
    attrs :: list(nakai@html@attrs:attr(any())),
    children :: list(nakai@html:node_(any()))
}).
