import gleam/bit_builder.{BitBuilder}
import gleam/dynamic.{Dynamic}
import gleam/erlang/atom.{Atom}
import gleam/erlang/process.{Pid}
import gleam/list
import gleam/map
import glisten/socket.{ListenSocket, Socket, SocketReason}
import glisten/socket/options

pub external fn controlling_process(
  socket: Socket,
  pid: Pid,
) -> Result(Nil, Atom) =
  "ssl_ffi" "controlling_process"

external fn do_listen(
  port: Int,
  options: List(options.TcpOption),
) -> Result(ListenSocket, SocketReason) =
  "ssl" "listen"

pub external fn accept_timeout(
  socket: ListenSocket,
  timeout: Int,
) -> Result(Socket, SocketReason) =
  "ssl" "transport_accept"

pub external fn accept(socket: ListenSocket) -> Result(Socket, SocketReason) =
  "ssl" "transport_accept"

pub external fn receive_timeout(
  socket: Socket,
  length: Int,
  timeout: Int,
) -> Result(BitString, SocketReason) =
  "ssl" "recv"

pub external fn receive(
  socket: Socket,
  length: Int,
) -> Result(BitString, SocketReason) =
  "ssl" "recv"

pub external fn send(
  socket: Socket,
  packet: BitBuilder,
) -> Result(Nil, SocketReason) =
  "ssl_ffi" "send"

pub external fn close(socket: Socket) -> Result(Nil, SocketReason) =
  "ssl_ffi" "close"

pub external fn do_shutdown(
  socket: Socket,
  write: Atom,
) -> Result(Nil, SocketReason) =
  "ssl_ffi" "shutdown"

pub fn shutdown(socket: Socket) -> Result(Nil, SocketReason) {
  let assert Ok(write) = atom.from_string("write")
  do_shutdown(socket, write)
}

external fn do_set_opts(socket: Socket, opts: List(Dynamic)) -> Result(Nil, Nil) =
  "ssl_ffi" "set_opts"

/// Update the optons for a socket (mutates the socket)
pub fn set_opts(
  socket: Socket,
  opts: List(options.TcpOption),
) -> Result(Nil, Nil) {
  opts
  |> options.to_map
  |> map.to_list
  |> list.map(dynamic.from)
  |> do_set_opts(socket, _)
}

pub external fn handshake(socket: Socket) -> Result(Socket, Nil) =
  "ssl" "handshake"

/// Start listening over SSL on a port with the given options
pub fn listen(
  port: Int,
  options: List(options.TcpOption),
) -> Result(ListenSocket, SocketReason) {
  options
  |> options.merge_with_defaults
  |> do_listen(port, _)
}

pub external fn negotiated_protocol(socket: Socket) -> Result(String, String) =
  "ssl_ffi" "negotiated_protocol"
