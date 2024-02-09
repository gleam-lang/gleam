//// A task is a kind of process that performs a single task and then shuts
//// down. Commonly tasks are used to convert sequential code into concurrent
//// code by performing computation in another process.
////
////    let task = task.async(fn() { do_some_work() })
////    let value = do_some_other_work()
////    value + task.await(task, 100)
////
//// Tasks spawned with async can be awaited on by their caller process (and
//// only their caller) as shown in the example above. They are implemented by
//// spawning a process that sends a message to the caller once the given
//// computation is performed.
////
//// There are two important things to consider when using `async`:
////
//// 1. If you are using async tasks, you must await a reply as they are always
////    sent.
////
//// 2. async tasks link the caller and the spawned process. This means that,
////    if the caller crashes, the task will crash too and vice-versa. This is
////    on purpose: if the process meant to receive the result no longer
////    exists, there is no purpose in completing the computation.
////
//// This module is inspired by Elixir's [Task module][1].
////
//// [1]: https://hexdocs.pm/elixir/master/Task.html
////

// TODO: await_many
import gleam/erlang/process.{type Pid, type ProcessMonitor, type Selector}
import gleam/dynamic.{type Dynamic}

pub opaque type Task(value) {
  Task(
    owner: Pid,
    pid: Pid,
    monitor: ProcessMonitor,
    selector: Selector(Message(value)),
  )
}

// TODO: test
/// Spawn a task process that calls a given function in order to perform some
/// work. The result of this function is send back to the parent and can be
/// received using the `await` function.
///
/// See the top level module documentation for more information on async/await.
///
pub fn async(work: fn() -> value) -> Task(value) {
  let owner = process.self()
  let subject = process.new_subject()
  let pid =
    process.start(linked: True, running: fn() { process.send(subject, work()) })
  let monitor = process.monitor_process(pid)
  let selector =
    process.new_selector()
    |> process.selecting_process_down(monitor, FromMonitor)
    |> process.selecting(subject, FromSubject)
  Task(owner: owner, pid: pid, monitor: monitor, selector: selector)
}

pub type AwaitError {
  Timeout
  Exit(reason: Dynamic)
}

// We can only wait on a task if we are the owner of it so crash if we are
// waiting on a task we don't own.
fn assert_owner(task: Task(a)) -> Nil {
  let self = process.self()
  case task.owner == self {
    True -> Nil
    False ->
      process.send_abnormal_exit(
        self,
        "awaited on a task that does not belong to this process",
      )
  }
}

type Message(value) {
  FromMonitor(process.ProcessDown)
  FromSubject(value)
}

// TODO: test
/// Wait for the value computed by a task.
///
/// If the a value is not received before the timeout has elapsed or if the
/// task process crashes then an error is returned.
///
pub fn try_await(task: Task(value), timeout: Int) -> Result(value, AwaitError) {
  assert_owner(task)
  case process.select(task.selector, timeout) {
    // The task process has sent back a value
    Ok(FromSubject(x)) -> {
      process.demonitor_process(task.monitor)
      Ok(x)
    }

    // The task process crashed without sending a value
    Ok(FromMonitor(process.ProcessDown(reason: reason, ..))) ->
      Error(Exit(reason))

    // The task process is alive but has not sent a value yet
    Error(Nil) -> Error(Timeout)
  }
}

// TODO: test
/// Wait for the value computed by a task.
///
/// If the a value is not received before the timeout has elapsed or if the
/// task process crashes then this function crashes.
///
pub fn await(task: Task(value), timeout: Int) -> value {
  let assert Ok(value) = try_await(task, timeout)
  value
}

/// Wait endlessly for the value computed by a task.
///
/// Be Careful! This function does not return until there is a value to
/// receive. If a value is not received then the process will be stuck waiting
/// forever.
///
pub fn try_await_forever(task: Task(value)) -> Result(value, AwaitError) {
  assert_owner(task)
  case process.select_forever(task.selector) {
    // The task process has sent back a value
    FromSubject(x) -> {
      process.demonitor_process(task.monitor)
      Ok(x)
    }

    // The task process crashed without sending a value
    FromMonitor(process.ProcessDown(reason: reason, ..)) -> Error(Exit(reason))
  }
}

/// Wait endlessly for the value computed by a task.
///
/// Be Careful! Like `try_await_forever`, this function does not return until there is a value to
/// receive.
///
/// If the task process crashes then this function crashes.
///
pub fn await_forever(task: Task(value)) -> value {
  let assert Ok(value) = try_await_forever(task)
  value
}
