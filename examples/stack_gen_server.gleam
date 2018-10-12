// A GenServer example taken from the Elixir documentation
// https://hexdocs.pm/elixir/GenServer.html#module-client-server-apis

import gen_server
import gen_server:Call
import gen_server:Cast
import gen_server:Init
import timeout:never

pub enum CastMsg(item) =
  | Push(item)
;

pub enum CallMsg =
  | Pop
;

pub enum Reply(item) =
  | Item(item)
  | Empty
;

// API

pub fn start_link(items) {
  gen_server:start_link(self, items)
}

pub fn push(pid, item) {
  gen_server:cast(pid, Push(item))
}

pub fn pop(pid) {
  gen_server:call(pid, Pop)
}

// callbacks

pub fn handle_call(call, _caller, items) =
  case (call, items) {
  | (Pop, x :: xs) => Call:Reply(Item(x), xs, never())
  | (Pop, []) => Call:Reply(Empty, [], never())
  }

pub fn handle_cast(cast, items) {
  case cast {
  | Push(item) => Cast:Ok(item :: items, never())
  }
}

pub fn handle_info(_info, items) {
  Cast:Noreply(items, never())
}

pub fn init(items) {
  Init:Ok(items, never())
}
