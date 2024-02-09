-record(file_info, {
    size :: integer(),
    file_type :: gleam@erlang@file:file_type(),
    access :: gleam@erlang@file:access(),
    atime :: integer(),
    mtime :: integer(),
    ctime :: integer(),
    mode :: integer(),
    links :: integer(),
    major_device :: integer(),
    minor_device :: integer(),
    inode :: integer(),
    user_id :: integer(),
    group_id :: integer()
}).
