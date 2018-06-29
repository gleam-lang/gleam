module RecordGenServer
  exposing Implementation, Sync(..), Async(..), Init(..), Caller, StartError,
    call/2, call/3, cast/2
    // Erlang gen_server behaviour callbacks
    handle_cast/3, handle_call/2, handle_info/2, info/1

// Gleam doesn't yet have a first-class module system, so to
// get type safety for GenServers we can wrap the gen_server
// module and pass records instead of module atoms.

import Foreign exposing Foreign

external type Caller

external foreign_start_link
  : |Atom, arg, List(Atom, Foreign)| -> Result(StartError, Pid)
  = :gen_server.start_link

external call : |Process, msg| -> reply = :gen_server.call

external cast : |Process, msg| -> () = :gen_server.cast

// Need to add others here
type StartError =
  | AlreadyStarted(Pid)

type StopReason =
  | Normal
  | Error(Atom)

type Timeout =
  | Hibernate
  | Timeout(Int)
  | None

type Sync(reply, state) =
  | Reply(reply, state, Timeout)
  | Ignore(state, Timeout)
  | ReplyStop(StopReason, reply, state)
  | IgnoreStop(StopReason, state)

type Async(state) =
  | Continue(state, Timeout)
  | Stop(StopReason, state)

type Init(state) =
  | Start(state, Timeout)
  | NoStart(StopReason)
  | Ignore

type alias State(error, state) {
  {
    impl_state :: state,
    impl :: Implementation(error, state),
  }
}

type alias Impl(arg, call, reply, cast, info, error, state) {
  {
    handle_call :: |call, Caller, state| -> Sync(reply, state)
    handle_cast :: |cast, state| -> Async(state),
    handle_info :: |info, state| -> Async(state),
    init :: |arg| -> Init(state),
  }
}

spec |Impl(arg, _, _, _, _, _, _), arg| -> Result(Atom, Pid)
fn start_link(implementation, arg) {
  Error(:not_implemented) // TODO
}

nodoc
fn handle_call(call, caller, state) {
  case state.impl.handle_call(call, state.impl_state) {
  | Reply(reply, impl_state, t) =>
      new_state = { state | impl_state = impl_state }
      Foreign.new({:reply, reply, new_state, timeout(t)})
  | Ignore(impl_state, t) =>
      new_state = { state | impl_state = impl_state }
      Foreign.new({:noreply, new_state, timeout(t)})
  | ReplyStop(reason, reply, impl_state) =>
      new_state = { state | impl_state = impl_state }
      Foreign.new({:stop, reason, reply, new_state})
  | IgnoreStop(reason, impl_state) =>
      new_state = { state | impl_state = impl_state }
      Foreign.new({:stop, reason, new_state})
  }
}

nodoc
fn handle_cast(cast, state, extra) {
  cast
    |> state.impl.handle_cast(_, state.impl_state)
    |> async_response
}

nodoc
fn handle_info(info, state, extra) {
  info
    |> state.impl.handle_info(_, state.impl_state)
    |> async_response
}

nodoc
fn init(arg) {
  case arg.impl.init(arg.impl_arg) {
  | Start(impl_state, t) =>
      state = { impl = arg.impl, impl_state = impl_state}
      Foreign.new({:ok, state, timeout(t)})
  | NoStart(reason) => Foreign.new({:stop, reason})
  | Ignore => Foreign.new(:ignore)
  }
}

fn aysnc_response(resp) {
  case resp {
  | Continue(impl_state, t) =>
      new_state = { state | impl_state = impl_state }
      Foreign.new({:noreply, new_state, timeout(t)})
  | Stop(reason, impl_state) =>
      new_state = { state | impl_state = impl_state }
      Foreign.new({:stop, reason, new_state})
  }
}

fn timeout(t)
  case t {
  | None => Foreign.new(:infinity)
  | Timeout(ms) => Foreign.new(ms)
  | Hibernate => Foreign.new(:hibernate)
  }
}
