import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}

pub type Mode {
  Running
  Suspended
}

pub type DebugOption {
  NoDebug
}

pub type DebugState

@external(erlang, "sys", "debug_options")
pub fn debug_state(a: List(DebugOption)) -> DebugState

pub type StatusInfo {
  StatusInfo(
    module: Atom,
    parent: Pid,
    mode: Mode,
    debug_state: DebugState,
    state: Dynamic,
  )
}

// TODO: document
// TODO: implement remaining messages
pub type SystemMessage {
  // {replace_state, StateFn}
  // {change_code, Mod, Vsn, Extra}
  // {terminate, Reason}
  // {debug, {log, Flag}}
  // {debug, {trace, Flag}}
  // {debug, {log_to_file, FileName}}
  // {debug, {statistics, Flag}}
  // {debug, no_debug}
  // {debug, {install, {Func, FuncState}}}
  // {debug, {install, {FuncId, Func, FuncState}}}
  // {debug, {remove, FuncOrId}}
  Resume(fn() -> Nil)
  Suspend(fn() -> Nil)
  GetState(fn(Dynamic) -> Nil)
  GetStatus(fn(StatusInfo) -> Nil)
}

type DoNotLeak

/// Get the state of a given OTP compatible process. This function is only
/// intended for debugging.
///
/// For more information see the [Erlang documentation][1].
///
/// [1]: https://erlang.org/doc/man/sys.html#get_state-1
///
@external(erlang, "sys", "get_state")
pub fn get_state(from from: Pid) -> Dynamic

@external(erlang, "sys", "suspend")
fn erl_suspend(a: Pid) -> DoNotLeak

/// Request an OTP compatible process to suspend, causing it to only handle
/// system messages.
///
/// For more information see the [Erlang documentation][1].
///
/// [1]: https://erlang.org/doc/man/sys.html#suspend-1
///
pub fn suspend(pid: Pid) -> Nil {
  erl_suspend(pid)
  Nil
}

@external(erlang, "sys", "resume")
fn erl_resume(from from: Pid) -> DoNotLeak

/// Request a suspended OTP compatible process to result, causing it to handle
/// all messages rather than only system messages.
///
/// For more information see the [Erlang documentation][1].
///
/// [1]: https://erlang.org/doc/man/sys.html#resume-1
///
pub fn resume(pid: Pid) -> Nil {
  erl_resume(pid)
  Nil
}
