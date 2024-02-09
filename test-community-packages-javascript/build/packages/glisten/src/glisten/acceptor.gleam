import gleam/erlang/process.{Abnormal, Subject}
import gleam/function
import gleam/iterator
import gleam/option.{None, Option, Some}
import gleam/otp/actor
import gleam/otp/supervisor
import gleam/result
import glisten/handler.{Handler, HandlerMessage, LoopFn, Ready}
import glisten/logger
import glisten/socket.{ListenSocket, Socket}
import glisten/socket/transport.{Transport}

pub type AcceptorMessage {
  AcceptConnection(ListenSocket)
}

pub type AcceptorError {
  AcceptError
  HandlerError
  ControlError
}

pub type AcceptorState {
  AcceptorState(
    sender: Subject(AcceptorMessage),
    socket: Option(Socket),
    transport: Transport,
  )
}

/// Worker process that handles `accept`ing connections and starts a new process
/// which receives the messages from the socket
pub fn start(
  pool: Pool(data),
) -> Result(Subject(AcceptorMessage), actor.StartError) {
  actor.start_spec(actor.Spec(
    init: fn() {
      let subject = process.new_subject()
      let selector =
        process.new_selector()
        |> process.selecting(subject, function.identity)

      process.send(subject, AcceptConnection(pool.listener_socket))

      actor.Ready(AcceptorState(subject, None, pool.transport), selector)
    },
    // TODO:  rethink this value, probably...
    init_timeout: 1000,
    loop: fn(msg, state) {
      let AcceptorState(sender, ..) = state
      case msg {
        AcceptConnection(listener) -> {
          let res = {
            use sock <- result.then(
              state.transport.accept(listener)
              |> result.replace_error(AcceptError),
            )
            use start <- result.then(
              Handler(
                sock,
                pool.initial_data,
                pool.handler,
                pool.on_init,
                pool.on_close,
                pool.transport,
              )
              |> handler.start
              |> result.replace_error(HandlerError),
            )
            sock
            |> state.transport.controlling_process(process.subject_owner(start))
            |> result.replace_error(ControlError)
            |> result.map(fn(_) { process.send(start, Ready) })
          }
          case res {
            Error(reason) -> {
              logger.error(#("Failed to accept/start handler", reason))
              actor.Stop(Abnormal("Failed to accept/start handler"))
            }
            _val -> {
              actor.send(sender, AcceptConnection(listener))
              actor.Continue(state)
            }
          }
        }
        msg -> {
          logger.error(#("Unknown message type", msg))
          actor.Stop(process.Abnormal("Unknown message type"))
        }
      }
    },
  ))
}

pub type Pool(data) {
  Pool(
    listener_socket: ListenSocket,
    handler: LoopFn(data),
    initial_data: data,
    pool_count: Int,
    on_init: Option(fn(Subject(HandlerMessage)) -> Nil),
    on_close: Option(fn(Subject(HandlerMessage)) -> Nil),
    transport: Transport,
  )
}

/// Initialize acceptor pool where each handler has no state
pub fn new_pool(handler: LoopFn(Nil)) -> fn(ListenSocket) -> Pool(Nil) {
  fn(listener_socket) {
    Pool(
      listener_socket: listener_socket,
      handler: handler,
      initial_data: Nil,
      pool_count: 10,
      on_init: None,
      on_close: None,
      transport: transport.tcp(),
    )
  }
}

/// Initialize an acceptor pool where each handler holds some state
pub fn new_pool_with_data(
  handler: LoopFn(data),
  initial_data: data,
) -> fn(ListenSocket) -> Pool(data) {
  fn(listener_socket) {
    Pool(
      listener_socket: listener_socket,
      handler: handler,
      initial_data: initial_data,
      pool_count: 10,
      on_init: None,
      on_close: None,
      transport: transport.tcp(),
    )
  }
}

/// Add an `on_init` handler to the acceptor pool
pub fn with_init(
  make_pool: fn(ListenSocket) -> Pool(data),
  func: fn(Subject(HandlerMessage)) -> Nil,
) -> fn(ListenSocket) -> Pool(data) {
  fn(socket) {
    let pool = make_pool(socket)
    Pool(..pool, on_init: Some(func))
  }
}

/// Add an `on_close` handler to the acceptor pool
pub fn with_close(
  make_pool: fn(ListenSocket) -> Pool(data),
  func: fn(Subject(HandlerMessage)) -> Nil,
) -> fn(ListenSocket) -> Pool(data) {
  fn(socket) {
    let pool = make_pool(socket)
    Pool(..pool, on_close: Some(func))
  }
}

/// Adjust the number of TCP acceptors in the pool
pub fn with_pool_size(
  make_pool: fn(ListenSocket) -> Pool(data),
  pool_count: Int,
) -> fn(ListenSocket) -> Pool(data) {
  fn(socket) {
    let pool = make_pool(socket)
    Pool(..pool, pool_count: pool_count)
  }
}

/// Use SSL for the underlying socket.
pub fn over_ssl(
  make_pool: fn(ListenSocket) -> Pool(data),
) -> fn(ListenSocket) -> Pool(data) {
  fn(socket) {
    let pool = make_pool(socket)
    Pool(..pool, transport: transport.ssl())
  }
}

/// Starts a pool of acceptors of size `pool_count`.
///
/// Runs `loop_fn` on ever message received
pub fn start_pool(
  pool: Pool(data),
) -> Result(Subject(supervisor.Message), actor.StartError) {
  supervisor.start_spec(supervisor.Spec(
    argument: Nil,
    // TODO:  i think these might need some tweaking
    max_frequency: 100,
    frequency_period: 1,
    init: fn(children) {
      iterator.range(from: 0, to: pool.pool_count)
      |> iterator.fold(
        children,
        fn(children, _index) {
          supervisor.add(children, supervisor.worker(fn(_arg) { start(pool) }))
        },
      )
    },
  ))
}
