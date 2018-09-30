// A GenServer example taken from the Elixir documentation
// https://hexdocs.pm/elixir/GenServer.html#module-client-server-apis

import GenServer:Call
import GenServer:Cast
import GenServer:Init
import Timeout:never

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
  | (Pop, x :: xs) => Call:Reply(Item(x), xs, never())
  | (Pop, []) => Call:Reply(Empty, [], never())
  }

fn handle_cast(cast, items) {
  case cast {
  | Push(item) => Cast:Ok(item :: items, never())
  }
}

fn handle_info(_info, items) {
  Cast:Noreply(items, never())
}

fn init(items) {
  Init:Ok(items, never())
}
