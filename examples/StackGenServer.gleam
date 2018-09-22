// A GenServer example taken from the Elixir documentation
// https://hexdocs.pm/elixir/GenServer.html#module-client-server-apis

import GenServer:Call
import GenServer:Cast
import GenServer:Init

type CastMsg(item) =
  | Push(item)
;

type CallMsg =
  | Pop
;

type Reply(item) =
  | Item(item)
  | Empty
;

// API

fn start_link(items) {
  RecordGenServer.start_link(self, items)
}

fn push(pid, item) {
  RecordGenServer.cast(pid, Push(item))
}

fn pop(pid) {
  RecordGenServer.call(pid, Pop)
}

// callbacks

fn handle_call(call, _caller, items) =
  case (call, items) {
  | (Pop, x :: xs) => Call:Reply(Item(x), xs, None)
  | (Pop, []) => Call:Reply(Empty, [], None)
  }

fn handle_cast(cast, items) {
  case cast {
  | Push(item) => Cast:Continue(item :: items, None)
  }
}

fn handle_info(_info, items) {
  Cast:Continue(items, None)
}

fn init(items) {
  Init:Start(items, None)
}
