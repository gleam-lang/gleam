import gleam/bit_builder.{BitBuilder}
import gleam/dynamic.{Dynamic}
import gleam/erlang/atom.{Atom}
import gleam/erlang/process.{Pid}
import gleam/list
import gleam/map.{Map}
import glisten/socket.{ListenSocket, Socket, SocketReason}
import glisten/socket/options.{TcpOption}

pub external fn controlling_process(
  socket: Socket,
  pid: Pid,
) -> Result(Nil, Atom) =
  "tcp_ffi" "controlling_process"

external fn do_listen_tcp(
  port: Int,
  options: List(TcpOption),
) -> Result(ListenSocket, SocketReason) =
  "gen_tcp" "listen"

pub external fn accept_timeout(
  socket: ListenSocket,
  timeout: Int,
) -> Result(Socket, SocketReason) =
  "gen_tcp" "accept"

pub external fn accept(socket: ListenSocket) -> Result(Socket, SocketReason) =
  "gen_tcp" "accept"

pub external fn receive_timeout(
  socket: Socket,
  length: Int,
  timeout: Int,
) -> Result(BitString, SocketReason) =
  "gen_tcp" "recv"

pub external fn receive(
  socket: Socket,
  length: Int,
) -> Result(BitString, SocketReason) =
  "gen_tcp" "recv"

pub external fn send(
  socket: Socket,
  packet: BitBuilder,
) -> Result(Nil, SocketReason) =
  "tcp_ffi" "send"

pub external fn socket_info(socket: Socket) -> Map(a, b) =
  "socket" "info"

pub external fn close(socket: a) -> Result(Nil, SocketReason) =
  "tcp_ffi" "close"

pub external fn do_shutdown(
  socket: Socket,
  write: Atom,
) -> Result(Nil, SocketReason) =
  "tcp_ffi" "shutdown"

pub fn shutdown(socket: Socket) -> Result(Nil, SocketReason) {
  let assert Ok(write) = atom.from_string("write")
  do_shutdown(socket, write)
}

external fn do_set_opts(socket: Socket, opts: List(Dynamic)) -> Result(Nil, Nil) =
  "tcp_ffi" "set_opts"

/// Update the optons for a socket (mutates the socket)
pub fn set_opts(socket: Socket, opts: List(TcpOption)) -> Result(Nil, Nil) {
  opts
  |> options.to_map
  |> map.to_list
  |> list.map(dynamic.from)
  |> do_set_opts(socket, _)
}

/// Start listening over TCP on a port with the given options
pub fn listen(
  port: Int,
  options: List(TcpOption),
) -> Result(ListenSocket, SocketReason) {
  options
  |> options.merge_with_defaults
  |> do_listen_tcp(port, _)
}

pub fn handshake(socket: Socket) -> Result(Socket, Nil) {
  Ok(socket)
}

pub external fn negotiated_protocol(socket: Socket) -> a =
  "tcp" "negotiated_protocol"
