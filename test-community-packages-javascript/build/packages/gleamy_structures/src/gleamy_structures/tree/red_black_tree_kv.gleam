// Based on "Deletion: The curse of the red-black tree" by Germane (2014)

import gleam/order.{Eq, Gt, Lt, Order}

type Color {
  R
  B
  BB
}

type Node(k, v) {
  E
  EE
  T(c: Color, l: Node(k, v), k: #(k, v), r: Node(k, v))
}

pub opaque type Tree(k, v) {
  Tree(root: Node(k, v), compare: fn(k, k) -> Order)
}

pub fn new(compare: fn(k, k) -> Order) -> Tree(k, v) {
  Tree(E, compare)
}

pub fn clear(tree: Tree(k, v)) -> Tree(k, v) {
  Tree(E, tree.compare)
}

pub fn insert(tree: Tree(k, v), key: k, value: v) -> Tree(k, v) {
  Tree(blacken(ins(tree.root, #(key, value), tree.compare)), tree.compare)
}

pub fn delete(tree: Tree(k, v), key: k) -> Tree(k, v) {
  Tree(del(redden(tree.root), key, tree.compare), tree.compare)
}

pub fn find(tree: Tree(k, v), key: k) -> Result(#(k, v), Nil) {
  do_find(tree.root, key, tree.compare)
}

pub fn fold(tree: Tree(k, v), acc: b, fun: fn(b, k, v) -> b) -> b {
  do_fold(tree.root, acc, fun)
}

pub fn foldr(tree: Tree(k, v), acc: b, fun: fn(b, k, v) -> b) -> b {
  do_foldr(tree.root, acc, fun)
}

pub fn draw(tree: Tree(k, v), to_string: fn(k, v) -> String) -> String {
  do_draw(tree.root, 0, to_string)
}

fn ins(node: Node(k, v), x: #(k, v), compare: fn(k, k) -> Order) -> Node(k, v) {
  case node {
    E -> T(R, E, x, E)
    T(c, k, y, b) ->
      case compare(x.0, y.0) {
        Lt -> balance(c, ins(k, x, compare), y, b)
        Gt -> balance(c, k, y, ins(b, x, compare))
        Eq -> T(c, k, x, b)
      }
    _ -> node
  }
}

fn blacken(node: Node(k, v)) -> Node(k, v) {
  case node {
    T(R, T(R, _, _, _) as l, y, c) -> T(B, l, y, c)
    T(R, k, x, T(R, _, _, _) as r) -> T(B, k, x, r)
    t -> t
  }
}

fn balance(c: Color, l: Node(k, v), v: #(k, v), r: Node(k, v)) -> Node(k, v) {
  case c, l, v, r {
    B, T(R, T(R, k, x, b), y, c), z, d -> T(R, T(B, k, x, b), y, T(B, c, z, d))
    B, T(R, k, x, T(R, b, y, c)), z, d -> T(R, T(B, k, x, b), y, T(B, c, z, d))
    B, k, x, T(R, T(R, b, y, c), z, d) -> T(R, T(B, k, x, b), y, T(B, c, z, d))
    B, k, x, T(R, b, y, T(R, c, z, d)) -> T(R, T(B, k, x, b), y, T(B, c, z, d))
    BB, k, x, T(R, T(R, b, y, c), z, d) -> T(B, T(B, k, x, b), y, T(B, c, z, d))
    BB, T(R, k, x, T(R, b, y, c)), z, d -> T(B, T(B, k, x, b), y, T(B, c, z, d))
    c, k, x, b -> T(c, k, x, b)
  }
}

fn redden(node: Node(k, v)) -> Node(k, v) {
  case node {
    T(B, T(B, _, _, _) as l, y, T(B, _, _, _) as r) -> T(R, l, y, r)
    t -> t
  }
}

fn del(node: Node(k, v), x: k, compare: fn(k, k) -> Order) -> Node(k, v) {
  case node {
    E -> node
    T(R, E, y, E) ->
      case compare(x, y.0) {
        Eq -> E
        _ -> node
      }
    T(B, E, y, E) ->
      case compare(x, y.0) {
        Eq -> EE
        _ -> node
      }
    T(B, T(R, E, y, E) as l, z, E) ->
      case compare(x, z.0) {
        Lt -> T(B, del(l, x, compare), z, E)
        Gt -> node
        Eq -> T(B, E, y, E)
      }
    T(c, k, y, b) ->
      case compare(x, y.0) {
        Lt -> rotate(c, del(k, x, compare), y, b)
        Gt -> rotate(c, k, y, del(b, x, compare))
        Eq ->
          case min_del(b) {
            Min(y1, b1) -> rotate(c, k, y1, b1)
            None -> E
          }
      }
    _ -> node
  }
}

fn rotate(c: Color, l: Node(k, v), v: #(k, v), r: Node(k, v)) -> Node(k, v) {
  case c, l, v, r {
    R, T(BB, k, x, b), y, T(B, c, z, d) ->
      balance(B, T(R, T(B, k, x, b), y, c), z, d)
    R, EE, y, T(B, c, z, d) -> balance(B, T(R, E, y, c), z, d)
    R, T(B, k, x, b), y, T(BB, c, z, d) ->
      balance(B, k, x, T(R, b, y, T(B, c, z, d)))
    R, T(B, k, x, b), y, EE -> balance(B, k, x, T(R, b, y, E))
    B, T(BB, k, x, b), y, T(B, c, z, d) ->
      balance(BB, T(R, T(B, k, x, b), y, c), z, d)
    B, EE, y, T(B, c, z, d) -> balance(BB, T(R, E, y, c), z, d)
    B, T(B, k, x, b), y, T(BB, c, z, d) ->
      balance(BB, k, x, T(R, b, y, T(B, c, z, d)))
    B, T(B, k, x, b), y, EE -> balance(BB, k, x, T(R, b, y, E))
    B, T(BB, k, w, b), x, T(R, T(B, c, y, d), z, e) ->
      T(B, balance(B, T(R, T(B, k, w, b), x, c), y, d), z, e)
    B, EE, x, T(R, T(B, c, y, d), z, e) ->
      T(B, balance(B, T(R, E, x, c), y, d), z, e)
    B, T(R, k, w, T(B, b, x, c)), y, T(BB, d, z, e) ->
      T(B, k, w, balance(B, b, x, T(R, c, y, T(B, d, z, e))))
    B, T(R, k, w, T(B, b, x, c)), y, EE ->
      T(B, k, w, balance(B, b, x, T(R, c, y, E)))
    c, k, x, b -> T(c, k, x, b)
  }
}

type MinDel(k, v) {
  Min(#(k, v), Node(k, v))
  None
}

fn min_del(node: Node(k, v)) -> MinDel(k, v) {
  case node {
    T(R, E, x, E) -> Min(x, E)
    T(B, E, x, E) -> Min(x, EE)
    T(B, E, x, T(R, E, y, E)) -> Min(x, T(B, E, y, E))
    T(c, k, x, b) ->
      case min_del(k) {
        Min(x1, a1) -> Min(x1, rotate(c, a1, x, b))
        None -> None
      }
    _ -> None
  }
}

fn do_find(
  node: Node(k, v),
  key: k,
  compare: fn(k, k) -> Order,
) -> Result(#(k, v), Nil) {
  case node {
    T(_, l, k, r) ->
      case compare(key, k.0) {
        Lt -> do_find(l, key, compare)
        Gt -> do_find(r, key, compare)
        Eq -> Ok(k)
      }
    _ -> Error(Nil)
  }
}

fn do_fold(node: Node(k, v), acc: a, fun: fn(a, k, v) -> a) -> a {
  case node {
    T(_, r, v, l) -> {
      let acc = do_fold(r, acc, fun)
      let acc = fun(acc, v.0, v.1)
      let acc = do_fold(l, acc, fun)
      acc
    }
    _ -> acc
  }
}

fn do_foldr(node: Node(k, v), acc: a, fun: fn(a, k, v) -> a) -> a {
  case node {
    T(_, r, v, l) -> {
      let acc = do_foldr(l, acc, fun)
      let acc = fun(acc, v.0, v.1)
      let acc = do_foldr(r, acc, fun)
      acc
    }
    _ -> acc
  }
}

fn do_indent(acc: String, i: Int) -> String {
  case i {
    0 -> acc
    i -> do_indent(".  " <> acc, i - 1)
  }
}

fn do_draw(
  node: Node(k, v),
  indent: Int,
  to_string: fn(k, v) -> String,
) -> String {
  case node {
    T(_, l, k, r) -> {
      let ls = do_draw(l, indent + 1, to_string)
      let ks = do_indent(to_string(k.0, k.1) <> "\n", indent)
      let rs = do_draw(r, indent + 1, to_string)
      ls <> ks <> rs
    }
    _ -> ""
  }
}
