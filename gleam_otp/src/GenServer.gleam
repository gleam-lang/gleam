module GenServer
  exposing Sync(..), Async(..), Init(..), Caller, StartError,
    call/2, call/3, cast/2

import Timeout exposing Timeout
import Foreign exposing Foreign

external type Caller

behaviour type CallMsg

behaviour type CastMsg

behaviour type State

behaviour type Argument

behaviour type Reply

behaviour type Pid = Process

; // Fix GitHub syntax highlighting

external call : |Pid, CallMsg| ->  = :gen_server :call

external cast : |Pid, CastMsg| -> () = :gen_server :cast

// TODO: Need to add others here
type StartError =
  | AlreadyStarted(Pid)

; // Fix GitHub syntax highlighting

type StopReason =
  | Normal
  | Error(Atom)

; // Fix GitHub syntax highlighting

type Sync =
  | Reply(Reply, State, Timeout)
  | Ignore(State, Timeout) tag :noreply
  | ReplyStop(StopReason, Reply, State) tag :stop
  | IgnoreStop(StopReason, State) tag :stop

; // Fix GitHub syntax highlighting

type Async =
  | Continue(State, Timeout) tag :noreply
  | Stop(StopReason, State)

; // Fix GitHub syntax highlighting

type Init =
  | Start(State, Timeout) tag :ok
  | NoStart(StopReason) tag :stop
  | Ignore

; // Fix GitHub syntax highlighting

callback handle_call :: |CallMsg, Caller, State| -> Sync(Reply, State)

callback handle_cast :: |CastMsg, State| -> Async(State)

callback handle_info :: |Foreign, State| -> Async(State)

callback init :: |Argument| -> Init(State)

spec |implementation(Self), Argument| -> Result(Atom, Pid)
fn start_link(mod, arg) {
  Error(:not_implemented) // TODO
}
