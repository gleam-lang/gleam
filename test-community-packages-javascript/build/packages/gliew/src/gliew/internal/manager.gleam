import gleam/map.{Map}
import gleam/list
import gleam/option.{None, Option, Some}
import gleam/result
import gleam/otp/actor
import gleam/erlang/process.{Selector, Subject}
import gleam/http/request.{Request}
import nakai
import nakai/html
import nakai/html/attrs
import mist.{Body}
import gliew/internal/event.{Event, LiveMount}
import gliew/internal/worker.{WorkerMessage}
import gliew/internal/util.{random_hex_string}

const sess_id_prefix = "gliew-"

const csrf_prefix = "g-"

type LoopState {
  LoopState(sessions: Map(String, Session))
}

type Session {
  Session(
    id: String,
    csrf: String,
    selects: List(fn(Selector(WorkerMessage)) -> Selector(WorkerMessage)),
    tree: html.Node(Event),
    worker: Option(Subject(WorkerMessage)),
  )
}

pub type Message {
  ProcessTree(
    from: Subject(html.Node(Event)),
    request: Request(Body),
    tree: html.Node(Event),
  )
  GetWorker(
    from: Subject(Result(Subject(WorkerMessage), Nil)),
    id: String,
    csrf: String,
  )
}

pub fn start_manager() {
  actor.start(LoopState(sessions: map.new()), loop)
}

fn loop(message: Message, state: LoopState) -> actor.Next(LoopState) {
  case message {
    // Process tree
    ProcessTree(from, _, tree) ->
      case extract_selects([], tree) {
        // Regular static view
        [] -> {
          process.send(from, tree)

          actor.Continue(state)
        }
        selects -> {
          // Create a session ID
          let sess_id = sess_id_prefix <> random_hex_string(10)
          // Create a CSRF token
          let csrf = csrf_prefix <> random_hex_string(24)

          process.send(
            from,
            tree
            |> wrap_live_view(sess_id, csrf),
          )

          actor.Continue(LoopState(
            sessions: state.sessions
            |> map.insert(sess_id, Session(sess_id, csrf, selects, tree, None)),
          ))
        }
      }

    // Get worker for session
    GetWorker(from, id, csrf) -> {
      case
        state.sessions
        |> map.get(id)
      {
        // Found session
        Ok(sess) ->
          case
            sess
            |> validate_session(csrf, _)
            |> result.map(get_worker_from_sess)
            |> result.flatten
          {
            // We got a worker
            Ok(worker) -> {
              process.send(from, Ok(worker))

              state.sessions
              |> map.insert(id, Session(..sess, worker: Some(worker)))
              |> LoopState
              |> actor.Continue
            }

            // We failed to get a worker
            Error(Nil) -> {
              process.send(from, Error(Nil))
              actor.Continue(state)
            }
          }

        // Sessions does not exist
        Error(Nil) -> {
          process.send(from, Error(Nil))
          actor.Continue(state)
        }
      }
    }
  }
}

// Validates that the provided csrf token matches
// the session's csrf.
//
fn validate_session(csrf: String, sess: Session) {
  case sess {
    Session(csrf: token, ..) if csrf == token -> Ok(sess)
    _ -> Error(Nil)
  }
}

// Get a worker by either extracting it from the
// session or starting a new one.
//
fn get_worker_from_sess(sess: Session) {
  case sess.worker {
    // There's already a running worker we can
    // return.
    Some(worker) -> Ok(worker)
    // There isn't a runnig worker so we need
    // to start one.
    None ->
      case worker.start_worker(sess.selects) {
        // Worker started successfully
        Ok(worker) -> Ok(worker)
        // Worker failed starting
        Error(_) -> Error(Nil)
      }
  }
}

// Walks the node tree until it finds a `LiveMount` event and adds
// its select function to the list of of all select functions
// in the tree.
//
fn extract_selects(
  selects: List(fn(Selector(WorkerMessage)) -> Selector(WorkerMessage)),
  node: html.Node(Event),
) {
  case node {
    html.Element(_, attrs, children) ->
      case extract_event(attrs) {
        Ok(LiveMount(selector)) ->
          selects
          |> list.prepend(selector)
        Error(Nil) ->
          children
          |> list.fold(selects, extract_selects)
      }
    html.LeafElement(_, attrs) ->
      case extract_event(attrs) {
        Ok(LiveMount(selector)) ->
          selects
          |> list.prepend(selector)
        Error(Nil) -> selects
      }
    _ -> selects
  }
}

// Extract a single gliew event attribute.
//
fn extract_event(attrs: List(attrs.Attr(Event))) {
  attrs
  |> list.find_map(fn(attr) {
    case attr {
      attrs.Event("gliew-event", event) -> Ok(event)
      _ -> Error(Nil)
    }
  })
}

// Wrap a container around a node tree containing any
// live mounts inside.
// This instructs htmx to make a websocket connection back
// to the server.
//
fn wrap_live_view(markup: html.Node(Event), session_id: String, csrf: String) {
  html.div(
    [
      attrs.Attr("hx-ext", "ws"),
      attrs.Attr(
        "ws-connect",
        "/connect?session=" <> session_id <> "&csrf=" <> csrf,
      ),
      attrs.Attr("hx-ext", "morph"),
    ],
    [markup],
  )
}

pub fn process_tree(
  subject: Subject(Message),
  request: Request(Body),
  tree: html.Node(Event),
) {
  process.call(subject, ProcessTree(_, request, tree), 1000)
}

pub fn get_worker(manager: Subject(Message), id: String, csrf: String) {
  process.call(manager, GetWorker(_, id, csrf), 1000)
}
