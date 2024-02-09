import gleam/erlang/process.{Selector}
import gliew/internal/worker.{WorkerMessage}

// Event is a special type that can be added to attributes
// in HTML tree elements.
// That way we can just return a HTML tree of `html.Node(Event)`
// and later walk the tree to extract various data from it.
//
pub type Event {
  // Instructs that the HTML node is the root of a mount
  // and contains the selector function for the worker
  // to know how to process updates.
  LiveMount(selecting: fn(Selector(WorkerMessage)) -> Selector(WorkerMessage))
  // Instructs us to add a `hx-swap-oob="morph"` to the
  // mount.
  Morph
  // Instruct us to add `hx-swap-oob="beforeend"` to the
  // mount.
  Append
}
