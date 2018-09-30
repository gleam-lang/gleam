import Timeout:Timeout
import Foreign:Foreign
import Process:Pid

pub external type Caller
;

behaviour GenServer {
  type CallMsg

  type CastMsg

  type State

  type Argument

  type Reply
  ;

  fn handle_call(CallMsg, Caller, State) { Call(Reply, State) }

  fn handle_cast(CastMsg, State) { Cast(State) }

  fn handle_info(Foreign, State) { Cast(State) }

  fn init(Argument) { Init(State) }
}

pub external fn call(Pid, CallMsg) { a } = 'gen_server' 'call'

pub external fn cast(Pid, CastMsg) { Unit } = 'gen_server' 'cast'

// TODO: Need to add others here
pub type StartError =
  | AlreadyStarted(Pid)
;

pub type StopReason =
  | Normal
  | Error(Atom)
;

pub type Call =
  | Reply(Reply, State, Timeout)
  | Noreply(State, Timeout)
  | Ignore(StopReason, State)
;

pub type Cast =
  | Noreply(State, Timeout)
  | Stop(StopReason, State)
;

pub type Init =
  | Ok(State, Timeout)
  | Stop(StopReason)
  | Ignore
;

fn start_link(mod: GenServer, arg: Argument) {
  throw('not_implemented') // TODO
}
