module StackRecordGenServer exposing start_link/1

// A GenServer example taken from the Elixir documentation
// https://hexdocs.pm/elixir/GenServer.html#module-client-server-apis

import RecordGenServer exposing Impl, Sync(..), Async(..), Init(..)

type Cast(item) =
  | Push(item)

type Call =
  | Pop

type Reply(item) =
  | Item(item)
  | Empty

// API

fn start_link(items) {
  RecordGenServer.start_link(implementation(), items)
}

fn push(pid, item) {
  RecordGenServer.cast(pid, Push(item))
}

fn pop(pid) {
  RecordGenServer.call(pid, Pop)
}

// Server (callbacks)

spec || -> Impl(List(item), Call, Reply, Cast, Never, Never, List(item))
fn implementation() {
  {
    handle_call = handle_call/3,
    handle_cast = handle_cast/2,
    handle_info = handle_info/2,
    init = init/1,
  }
}

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
