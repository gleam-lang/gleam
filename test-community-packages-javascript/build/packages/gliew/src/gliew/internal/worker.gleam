import gleam/io
import gleam/string
import gleam/list
import gleam/option.{None, Option, Some}
import gleam/function
import gleam/otp/actor
import gleam/erlang/process.{Selector, Subject, Timer}
import glisten/handler.{HandlerMessage}
import mist/websocket
import mist/internal/websocket.{TextMessage} as iws

// inactivity timeout of 10 minutes
const timeout = 600_000

type State {
  State(
    self: Subject(WorkerMessage),
    socket: Option(Subject(HandlerMessage)),
    timer: Option(Timer),
  )
}

pub type WorkerMessage {
  LiveUpdate(markup: String)
  ConnectSocket(socket: Subject(HandlerMessage))
  DisconnectSocket(socket: Subject(HandlerMessage))
  Shutdown
}

pub fn start_worker(
  selects: List(fn(Selector(WorkerMessage)) -> Selector(WorkerMessage)),
) {
  actor.start_spec(actor.Spec(
    init: fn() {
      let subject = process.new_subject()

      // Apply selectors from live mounts.
      let selector =
        process.new_selector()
        |> process.selecting(subject, function.identity)
        |> apply_selects(selects)

      // Send ourselves a shutdown timer that we will wait
      // for a socket for.
      let timer = process.send_after(subject, timeout, Shutdown)

      actor.Ready(State(subject, None, Some(timer)), selector)
    },
    init_timeout: 1000,
    loop: worker_loop,
  ))
}

fn worker_loop(msg: WorkerMessage, state: State) {
  case msg {
    // We got a new live mount update.
    LiveUpdate(markup) ->
      case state.socket {
        // We have a socket to send data to.
        Some(sock) -> {
          // Send updated markup to websocket.
          websocket.send(sock, TextMessage(markup))

          actor.Continue(state)
        }
        // No socket is connected to the worker.
        None -> actor.Continue(state)
      }
    // We got a socket to join.
    ConnectSocket(sock) -> {
      // Cancel timer
      case state.timer {
        Some(timer) -> {
          process.cancel_timer(timer)
          Nil
        }
        None -> Nil
      }

      // Update state
      actor.Continue(State(..state, socket: Some(sock), timer: None))
    }
    // We should disconnect from socket.
    DisconnectSocket(sock) ->
      case state.socket {
        // We have a socket and the socket matches
        // the one we should disconnect from.
        Some(socket) if sock == socket ->
          // Send ourselves a Shutdown message after timeout.
          process.send_after(state.self, timeout, Shutdown)
          |> Some
          |> State(state.self, None, _)
          |> actor.Continue
        // Either we don't have a socket or the
        // one provided doesn't match so we
        // ignore the message.
        _ -> actor.Continue(state)
      }
    // We got a shutdown message.
    Shutdown -> {
      io.println(
        "shutting down worker " <> string.inspect(process.subject_owner(
          state.self,
        )),
      )
      actor.Stop(process.Normal)
    }
  }
}

fn apply_selects(
  selector: Selector(WorkerMessage),
  selects: List(fn(Selector(WorkerMessage)) -> Selector(WorkerMessage)),
) {
  selector
  |> list.fold(selects, _, fn(selector, selecting) { selecting(selector) })
}

pub fn connect(worker: Subject(WorkerMessage), socket: Subject(HandlerMessage)) {
  process.send(worker, ConnectSocket(socket))
}

pub fn disconnect(
  worker: Subject(WorkerMessage),
  socket: Subject(HandlerMessage),
) {
  process.send(worker, DisconnectSocket(socket))
}
