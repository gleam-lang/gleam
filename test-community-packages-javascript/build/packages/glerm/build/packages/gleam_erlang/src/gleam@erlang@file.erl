-module(gleam@erlang@file).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export([file_info/1, link_info/1, is_directory/1, is_regular/1, file_exists/1, link_exists/1, make_directory/1, list_directory/1, delete_directory/1, recursive_delete/1, read/1, read_bits/1, write/2, write_bits/2, append/2, append_bits/2, delete/1]).
-export_type([reason/0, file_type/0, access/0, file_info/0]).

-type reason() :: eacces |
    eagain |
    ebadf |
    ebadmsg |
    ebusy |
    edeadlk |
    edeadlock |
    edquot |
    eexist |
    efault |
    efbig |
    eftype |
    eintr |
    einval |
    eio |
    eisdir |
    eloop |
    emfile |
    emlink |
    emultihop |
    enametoolong |
    enfile |
    enobufs |
    enodev |
    enolck |
    enolink |
    enoent |
    enomem |
    enospc |
    enosr |
    enostr |
    enosys |
    enotblk |
    enotdir |
    enotsup |
    enxio |
    eopnotsupp |
    eoverflow |
    eperm |
    epipe |
    erange |
    erofs |
    espipe |
    esrch |
    estale |
    etxtbsy |
    exdev |
    not_utf8.

-type file_type() :: device | directory | other | regular | symlink.

-type access() :: no_access | read | read_write | write.

-type file_info() :: {file_info,
        integer(),
        file_type(),
        access(),
        integer(),
        integer(),
        integer(),
        integer(),
        integer(),
        integer(),
        integer(),
        integer(),
        integer(),
        integer()}.

-spec file_info(binary()) -> {ok, file_info()} | {error, reason()}.
file_info(A) ->
    gleam_erlang_ffi:file_info(A).

-spec link_info(binary()) -> {ok, file_info()} | {error, reason()}.
link_info(A) ->
    gleam_erlang_ffi:link_info(A).

-spec is_directory(binary()) -> {ok, boolean()} | {error, reason()}.
is_directory(Path) ->
    gleam@result:map(
        gleam_erlang_ffi:file_info(Path),
        fun(_use0) ->
            {file_info, _, File_type, _, _, _, _, _, _, _, _, _, _, _} = _use0,
            File_type =:= directory
        end
    ).

-spec is_regular(binary()) -> {ok, boolean()} | {error, reason()}.
is_regular(Path) ->
    gleam@result:map(
        gleam_erlang_ffi:file_info(Path),
        fun(_use0) ->
            {file_info, _, File_type, _, _, _, _, _, _, _, _, _, _, _} = _use0,
            File_type =:= regular
        end
    ).

-spec file_exists(binary()) -> {ok, boolean()} | {error, reason()}.
file_exists(Path) ->
    Result = begin
        _pipe = Path,
        _pipe@1 = gleam_erlang_ffi:file_info(_pipe),
        gleam@result:replace(_pipe@1, true)
    end,
    case Result of
        {error, enoent} ->
            {ok, false};

        _ ->
            Result
    end.

-spec link_exists(binary()) -> {ok, boolean()} | {error, reason()}.
link_exists(Path) ->
    Result = begin
        _pipe = Path,
        _pipe@1 = gleam_erlang_ffi:link_info(_pipe),
        gleam@result:replace(_pipe@1, true)
    end,
    case Result of
        {error, enoent} ->
            {ok, false};

        _ ->
            Result
    end.

-spec make_directory(binary()) -> {ok, nil} | {error, reason()}.
make_directory(A) ->
    gleam_erlang_ffi:make_directory(A).

-spec list_directory(binary()) -> {ok, list(binary())} | {error, reason()}.
list_directory(A) ->
    gleam_erlang_ffi:list_directory(A).

-spec delete_directory(binary()) -> {ok, nil} | {error, reason()}.
delete_directory(A) ->
    gleam_erlang_ffi:delete_directory(A).

-spec recursive_delete(binary()) -> {ok, nil} | {error, reason()}.
recursive_delete(A) ->
    gleam_erlang_ffi:recursive_delete(A).

-spec read(binary()) -> {ok, binary()} | {error, reason()}.
read(Path) ->
    _pipe = Path,
    _pipe@1 = gleam_erlang_ffi:read_file(_pipe),
    gleam@result:then(
        _pipe@1,
        fun(Content) -> case gleam@bit_array:to_string(Content) of
                {ok, String} ->
                    {ok, String};

                {error, nil} ->
                    {error, not_utf8}
            end end
    ).

-spec read_bits(binary()) -> {ok, bitstring()} | {error, reason()}.
read_bits(Path) ->
    gleam_erlang_ffi:read_file(Path).

-spec write(binary(), binary()) -> {ok, nil} | {error, reason()}.
write(Contents, Path) ->
    _pipe = Contents,
    _pipe@1 = gleam_stdlib:identity(_pipe),
    gleam_erlang_ffi:write_file(_pipe@1, Path).

-spec write_bits(bitstring(), binary()) -> {ok, nil} | {error, reason()}.
write_bits(Contents, Path) ->
    gleam_erlang_ffi:write_file(Contents, Path).

-spec append(binary(), binary()) -> {ok, nil} | {error, reason()}.
append(Contents, Path) ->
    _pipe = Contents,
    _pipe@1 = gleam_stdlib:identity(_pipe),
    gleam_erlang_ffi:append_file(_pipe@1, Path).

-spec append_bits(bitstring(), binary()) -> {ok, nil} | {error, reason()}.
append_bits(Contents, Path) ->
    gleam_erlang_ffi:append_file(Contents, Path).

-spec delete(binary()) -> {ok, nil} | {error, reason()}.
delete(A) ->
    gleam_erlang_ffi:delete_file(A).
