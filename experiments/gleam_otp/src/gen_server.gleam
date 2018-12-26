// FIXME: There are overlapping enum constructors in this file.
// - Ignore
// - Stop
// - Noreply

import timeout:Timeout
import any:Any
import process:Pid

pub external type Caller;

pub enum StartResult =
  | Ignore
  | Ok(Pid)
  | Error(StartError)

// TODO: Need to add others here
pub enum StartError =
  | AlreadyStarted(Pid)

pub enum StopReason =
  | Normal
  | Error(Atom)

pub enum Call(reply, state) =
  | Reply(reply, state, Timeout)
  | Noreply(state, Timeout)
  | Ignore(StopReason, State)

pub enum Cast(state) =
  | Noreply(state, Timeout)
  | Stop(StopReason, state)

pub enum Init(state) =
  | Ok(state, Timeout)
  | Stop(StopReason)
  | Ignore

pub type GenServer(arg, state, call, cast, reply, r) =
  module { r |
    fn handle_call(call, Caller, state) -> Call(reply, state);

    fn handle_cast(cast, state) -> Cast(state);

    fn handle_info(Any, state) -> Cast(state);

    fn init(Argument) -> Init(State);
  };

external fn native_call(Pid, untyped_call) -> untyped_reply = 'gen_server' 'call'

pub fn call(mod: GenServer(_, _, call, _, reply, _), server: Pid, call: call) -> reply {
  native_call(server, call)
}

external fn native_cast(Pid, untyped_cast) -> Unit = 'gen_server' 'cast'

pub fn cast(mod: GenServer(_, _, _, cast, reply, _), server: Pid, cast: cast) {
  native_cast(server, call)
}

external fn native_start_link(GenServer(arg, _, _, _, _, _), arg, List(Tuple(Atom, Any)))
  -> StartResult
  = 'gen_server' 'start_link'

pub fn start_link(mod, arg) {
  // TODO: Optionally name server with gen_server:start_link/4

  // TODO: Allow options to be specified
  opts = []
  native_start_link(mod, arg, [])
}
