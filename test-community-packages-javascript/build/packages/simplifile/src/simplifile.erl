-module(simplifile).
-compile([no_auto_import, nowarn_unused_vars]).

-export([append_bits/2, append/2, write_bits/2, write/2, read_bits/1, read/1, delete/1, is_directory/1, list_contents/1]).
-export_type([file_error/0]).

-type file_error() :: eacces |
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
    not_utf8 |
    unknown.

-spec cast_error({ok, EXS} | {error, file_error()}) -> {ok, EXS} |
    {error, file_error()}.
cast_error(Input) ->
    Input.

-spec append_bits(bitstring(), binary()) -> {ok, nil} | {error, file_error()}.
append_bits(Bits, Filepath) ->
    _pipe = gleam_erlang_ffi:append_file(Bits, Filepath),
    cast_error(_pipe).

-spec do_append(binary(), binary()) -> {ok, nil} | {error, file_error()}.
do_append(Content, Filepath) ->
    _pipe = Content,
    _pipe@1 = gleam@bit_string:from_string(_pipe),
    gleam_erlang_ffi:append_file(_pipe@1, Filepath).

-spec append(binary(), binary()) -> {ok, nil} | {error, file_error()}.
append(Contents, Filepath) ->
    _pipe = do_append(Contents, Filepath),
    cast_error(_pipe).

-spec write_bits(bitstring(), binary()) -> {ok, nil} | {error, file_error()}.
write_bits(Bits, Filepath) ->
    _pipe = gleam_erlang_ffi:write_file(Bits, Filepath),
    cast_error(_pipe).

-spec do_write(binary(), binary()) -> {ok, nil} | {error, file_error()}.
do_write(Content, Filepath) ->
    _pipe = Content,
    _pipe@1 = gleam@bit_string:from_string(_pipe),
    gleam_erlang_ffi:write_file(_pipe@1, Filepath).

-spec write(binary(), binary()) -> {ok, nil} | {error, file_error()}.
write(Contents, Filepath) ->
    _pipe = do_write(Contents, Filepath),
    cast_error(_pipe).

-spec read_bits(binary()) -> {ok, bitstring()} | {error, file_error()}.
read_bits(Filepath) ->
    _pipe = gleam_erlang_ffi:read_file(Filepath),
    cast_error(_pipe).

-spec do_read(binary()) -> {ok, binary()} | {error, file_error()}.
do_read(Filepath) ->
    case gleam_erlang_ffi:read_file(Filepath) of
        {ok, Bit_str} ->
            case gleam@bit_string:to_string(Bit_str) of
                {ok, Str} ->
                    {ok, Str};

                _ ->
                    {error, not_utf8}
            end;

        {error, E} ->
            {error, E}
    end.

-spec read(binary()) -> {ok, binary()} | {error, file_error()}.
read(Filepath) ->
    _pipe = do_read(Filepath),
    cast_error(_pipe).

-spec delete(binary()) -> {ok, nil} | {error, file_error()}.
delete(Filepath) ->
    _pipe = gleam_erlang_ffi:delete_file(Filepath),
    cast_error(_pipe).

-spec is_directory(binary()) -> boolean().
is_directory(Filepath) ->
    filelib:is_dir(Filepath).

-spec list_contents(binary()) -> {ok, list(binary())} | {error, file_error()}.
list_contents(Directory) ->
    gleam_erlang_ffi:list_directory(Directory).
