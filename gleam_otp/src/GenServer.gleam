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

external call : fn(Pid, CallMsg) { a } = :gen_server :call

external cast : fn(Pid, CastMsg) { Unit } = :gen_server :cast

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

callback handle_call :: fn(CallMsg, Caller, State) { Sync(Reply, State) }

callback handle_cast :: fn(CastMsg, State) { Async(State) }

callback handle_info :: fn(Foreign, State) { Async(State) }

callback init :: fn(Argument) { Init(State) }

fn start_link(mod: module Self, arg: Argument) {
  Error(:not_implemented) // TODO
}
