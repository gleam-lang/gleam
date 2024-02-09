-module(glisten@socket).
-compile([no_auto_import, nowarn_unused_vars]).

-export_type([socket_reason/0, listen_socket/0, socket/0]).

-type socket_reason() :: closed |
    timeout |
    badarg |
    terminated |
    eaddrinuse |
    eaddrnotavail |
    eafnosupport |
    ealready |
    econnaborted |
    econnrefused |
    econnreset |
    edestaddrreq |
    ehostdown |
    ehostunreach |
    einprogress |
    eisconn |
    emsgsize |
    enetdown |
    enetunreach |
    enopkg |
    enoprotoopt |
    enotconn |
    enotty |
    enotsock |
    eproto |
    eprotonosupport |
    eprototype |
    esocktnosupport |
    etimedout |
    ewouldblock |
    exbadport |
    exbadseq |
    eacces |
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
    exdev.

-opaque listen_socket() :: listen_socket.

-opaque socket() :: socket.


