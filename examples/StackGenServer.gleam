module StackGenServer
  implementing GenServer
  exposing CallMsg, CastMsg, InfoMsg, State, Argument, Reply,
    start_link, handle_call, handle_cast, handle_info, init

// A GenServer example taken from the Elixir documentation
// https://hexdocs.pm/elixir/GenServer.html#module-client-server-apis

import GenServer exposing Sync(..), Async(..), Init(..)

type CastMsg(item) =
  | Push(item)

; // Fix GitHub syntax highlighting

type CallMsg =
  | Pop

; // Fix GitHub syntax highlighting

type Reply(item) =
  | Item(item)
  | Empty

; // Fix GitHub syntax highlighting

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
  | (Pop, x :: xs) => Reply(Item(x), xs, None)
  | (Pop, []) => Reply(Empty, [], None)
  }

fn handle_cast(cast, items) {
  case cast {
  | Push(item) => Continue(item :: items, None)
  }
}

fn handle_info(_info, items) {
  Continue(items, None)
}

fn init(items) {
  Start(items, None)
}
