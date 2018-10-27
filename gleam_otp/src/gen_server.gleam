import timeout:Timeout
import any:Any
import process:Pid

pub external type Caller
;

behaviour GenServer {
  type CallMsg

  type CastMsg

  type State

  type Argument

  type Reply
  ;

  fn handle_call(CallMsg, Caller, State) -> Call(Reply, State)

  fn handle_cast(CastMsg, State) -> Cast(State)

  fn handle_info(Any, State) -> Cast(State)

  fn init(Argument) -> Init(State)
}

pub external fn call(Pid, CallMsg) -> a = 'gen_server' 'call'

pub external fn cast(Pid, CastMsg) -> Unit = 'gen_server' 'cast'

// TODO: Need to add others here
pub enum StartError =
  | AlreadyStarted(Pid)

pub enum StopReason =
  | Normal
  | Error(Atom)

pub enum Call =
  | Reply(Reply, State, Timeout)
  | Noreply(State, Timeout)
  | Ignore(StopReason, State)

pub enum Cast =
  | Noreply(State, Timeout)
  | Stop(StopReason, State)

pub enum Init =
  | Ok(State, Timeout)
  | Stop(StopReason)
  | Ignore

fn start_link(mod: GenServer, arg: Argument) {
  throw('not_implemented') // TODO
}
