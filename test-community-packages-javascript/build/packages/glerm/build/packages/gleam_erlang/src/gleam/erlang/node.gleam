import gleam/erlang/atom.{type Atom}

pub type Node

type DoNotLeak

/// Return the current node.
///
@external(erlang, "erlang", "node")
pub fn self() -> Node

/// Return a list of all visible nodes in the cluster, not including the current
/// node.
///
/// The current node can be included by calling `self()` and prepending the
/// result.
///
/// ```gleam
/// let all_nodes = [node.self(), ..node.visible()]
/// ```
///
@external(erlang, "erlang", "nodes")
pub fn visible() -> List(Node)

pub type ConnectError {
  /// Was unable to connect to the node.
  FailedToConnect
  /// The local node is not alive, so it is not possible to connect to the other
  /// node.
  LocalNodeIsNotAlive
}

// TODO: test unknown node
// TODO: test successfully connecting
/// Establish a connection to a node, so the nodes can send messages to each
/// other and any other connected nodes.
///
/// Returns `Error(FailedToConnect)` if the node is not reachable.
///
/// Returns `Error(LocalNodeIsNotAlive)` if the local node is not alive, meaning
/// it is not running in distributed mode.
///
@external(erlang, "gleam_erlang_ffi", "connect_node")
pub fn connect(node: Atom) -> Result(Node, ConnectError)

// TODO: test
/// Send a message to a named process on a given node.
///
/// These messages are untyped, like regular Erlang messages.
///
pub fn send(node: Node, name: Atom, message: message) -> Nil {
  raw_send(#(name, node), message)
  Nil
}

@external(erlang, "erlang", "send")
fn raw_send(receiver: #(Atom, Node), message: message) -> DoNotLeak

/// Convert a node to the atom of its name.
///
@external(erlang, "gleam_erlang_ffi", "identity")
pub fn to_atom(node: Node) -> Atom
