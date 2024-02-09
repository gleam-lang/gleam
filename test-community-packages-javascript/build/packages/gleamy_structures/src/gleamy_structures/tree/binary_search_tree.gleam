import gleam/order.{Eq, Gt, Lt, Order}

type Node(a) {
  Empty
  Node(l: Node(a), k: a, r: Node(a))
}

pub opaque type Tree(a) {
  Tree(root: Node(a), compare: fn(a, a) -> Order)
}

pub fn new(compare: fn(a, a) -> Order) -> Tree(a) {
  Tree(Empty, compare)
}

pub fn clear(tree: Tree(a)) -> Tree(a) {
  Tree(Empty, tree.compare)
}

pub fn insert(tree: Tree(a), key: a) -> Tree(a) {
  Tree(do_insert(tree.root, key, tree.compare), tree.compare)
}

pub fn delete(tree: Tree(a), key: a) -> Tree(a) {
  Tree(do_delete(tree.root, key, tree.compare), tree.compare)
}

pub fn find(tree: Tree(a), key: a) -> Result(a, Nil) {
  do_find(tree.root, key, tree.compare)
}

pub fn fold(tree: Tree(a), acc: b, fun: fn(b, a) -> b) -> b {
  do_fold(tree.root, acc, fun)
}

pub fn draw(tree: Tree(a), to_string: fn(a) -> String) {
  do_draw(tree.root, 0, to_string)
}

fn do_insert(node, key, compare) {
  case node {
    Node(l, k, r) ->
      case compare(key, k) {
        Lt -> Node(do_insert(l, key, compare), k, r)
        Gt -> Node(l, k, do_insert(r, key, compare))
        Eq -> Node(l, key, r)
      }
    Empty -> Node(Empty, key, Empty)
  }
}

fn do_min(node, compare) {
  case node {
    Node(Node(_, _, _) as l, _, _) -> do_min(l, compare)
    Node(Empty, _, _) -> node
    Empty -> Empty
  }
}

fn do_delete(node, key, compare) {
  case node {
    Node(l, k, r) ->
      case compare(key, k) {
        Lt -> Node(do_delete(l, key, compare), k, r)
        Gt -> Node(l, k, do_delete(r, key, compare))
        Eq ->
          case node {
            Node(Empty, _, r) -> r
            Node(l, _, Empty) -> l
            Node(l, _, r) ->
              case do_min(r, compare) {
                Node(_, mk, _) -> Node(l, mk, do_delete(r, mk, compare))
                Empty -> Empty
              }
            Empty -> Empty
          }
      }
    Empty -> Empty
  }
}

fn do_find(node, key, compare) {
  case node {
    Node(l, k, r) ->
      case compare(key, k) {
        Lt -> do_find(l, key, compare)
        Gt -> do_find(r, key, compare)
        Eq -> Ok(k)
      }
    Empty -> Error(Nil)
  }
}

fn do_fold(node, acc, fun) {
  case node {
    Node(r, v, l) -> {
      let acc = do_fold(r, acc, fun)
      let acc = fun(acc, v)
      let acc = do_fold(l, acc, fun)
      acc
    }
    Empty -> acc
  }
}

fn do_indent(acc, i) {
  case i {
    0 -> acc
    i -> do_indent(".  " <> acc, i - 1)
  }
}

fn do_draw(node, indent, to_string) {
  case node {
    Node(l, k, r) -> {
      let ls = do_draw(l, indent + 1, to_string)
      let ks = do_indent(to_string(k) <> "\n", indent)
      let rs = do_draw(r, indent + 1, to_string)
      ls <> ks <> rs
    }
    Empty -> ""
  }
}
