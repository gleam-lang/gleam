// Based on "Deletion: The curse of the red-black tree" by Germane (2014)

import gleam/order.{Eq, Gt, Lt, Order}

type Color {
  R
  B
  BB
}

type Node(a) {
  E
  EE
  T(c: Color, l: Node(a), k: a, r: Node(a))
}

pub opaque type Tree(a) {
  Tree(root: Node(a), compare: fn(a, a) -> Order)
}

pub fn new(compare: fn(a, a) -> Order) -> Tree(a) {
  Tree(E, compare)
}

pub fn clear(tree: Tree(a)) -> Tree(a) {
  Tree(E, tree.compare)
}

pub fn insert(tree: Tree(a), key: a) -> Tree(a) {
  Tree(blacken(ins(tree.root, key, tree.compare)), tree.compare)
}

pub fn delete(tree: Tree(a), key: a) -> Tree(a) {
  Tree(del(redden(tree.root), key, tree.compare), tree.compare)
}

pub fn find(tree: Tree(a), key: a) -> Result(a, Nil) {
  do_find(tree.root, key, tree.compare)
}

pub fn fold(tree: Tree(a), acc: b, fun: fn(b, a) -> b) -> b {
  do_fold(tree.root, acc, fun)
}

pub fn foldr(tree: Tree(a), acc: b, fun: fn(b, a) -> b) -> b {
  do_foldr(tree.root, acc, fun)
}

pub fn draw(tree: Tree(a), to_string: fn(a) -> String) {
  do_draw(tree.root, 0, to_string)
}

fn ins(node, x, compare) {
  case node {
    E -> T(R, E, x, E)
    T(c, a, y, b) ->
      case compare(x, y) {
        Lt -> balance(c, ins(a, x, compare), y, b)
        Gt -> balance(c, a, y, ins(b, x, compare))
        Eq -> T(c, a, x, b)
      }
    _ -> node
  }
}

fn blacken(node: Node(a)) -> Node(a) {
  case node {
    T(R, T(R, _, _, _) as l, y, c) -> T(B, l, y, c)
    T(R, a, x, T(R, _, _, _) as r) -> T(B, a, x, r)
    t -> t
  }
}

fn balance(c: Color, l: Node(a), v: a, r: Node(a)) -> Node(a) {
  case c, l, v, r {
    B, T(R, T(R, a, x, b), y, c), z, d -> T(R, T(B, a, x, b), y, T(B, c, z, d))
    B, T(R, a, x, T(R, b, y, c)), z, d -> T(R, T(B, a, x, b), y, T(B, c, z, d))
    B, a, x, T(R, T(R, b, y, c), z, d) -> T(R, T(B, a, x, b), y, T(B, c, z, d))
    B, a, x, T(R, b, y, T(R, c, z, d)) -> T(R, T(B, a, x, b), y, T(B, c, z, d))
    BB, a, x, T(R, T(R, b, y, c), z, d) -> T(B, T(B, a, x, b), y, T(B, c, z, d))
    BB, T(R, a, x, T(R, b, y, c)), z, d -> T(B, T(B, a, x, b), y, T(B, c, z, d))
    c, a, x, b -> T(c, a, x, b)
  }
}

fn redden(node: Node(a)) -> Node(a) {
  case node {
    T(B, T(B, _, _, _) as l, y, T(B, _, _, _) as r) -> T(R, l, y, r)
    t -> t
  }
}

fn del(node, x, compare) {
  case node {
    E -> node
    T(R, E, y, E) ->
      case compare(x, y) {
        Eq -> E
        _ -> node
      }
    T(B, E, y, E) ->
      case compare(x, y) {
        Eq -> EE
        _ -> node
      }
    T(B, T(R, E, y, E) as l, z, E) ->
      case compare(x, z) {
        Lt -> T(B, del(l, x, compare), z, E)
        Gt -> node
        Eq -> T(B, E, y, E)
      }
    T(c, a, y, b) ->
      case compare(x, y) {
        Lt -> rotate(c, del(a, x, compare), y, b)
        Gt -> rotate(c, a, y, del(b, x, compare))
        Eq ->
          case min_del(b) {
            Min(y1, b1) -> rotate(c, a, y1, b1)
            None -> E
          }
      }
    _ -> node
  }
}

fn rotate(c: Color, l: Node(a), v: a, r: Node(a)) -> Node(a) {
  case c, l, v, r {
    R, T(BB, a, x, b), y, T(B, c, z, d) ->
      balance(B, T(R, T(B, a, x, b), y, c), z, d)
    R, EE, y, T(B, c, z, d) -> balance(B, T(R, E, y, c), z, d)
    R, T(B, a, x, b), y, T(BB, c, z, d) ->
      balance(B, a, x, T(R, b, y, T(B, c, z, d)))
    R, T(B, a, x, b), y, EE -> balance(B, a, x, T(R, b, y, E))
    B, T(BB, a, x, b), y, T(B, c, z, d) ->
      balance(BB, T(R, T(B, a, x, b), y, c), z, d)
    B, EE, y, T(B, c, z, d) -> balance(BB, T(R, E, y, c), z, d)
    B, T(B, a, x, b), y, T(BB, c, z, d) ->
      balance(BB, a, x, T(R, b, y, T(B, c, z, d)))
    B, T(B, a, x, b), y, EE -> balance(BB, a, x, T(R, b, y, E))
    B, T(BB, a, w, b), x, T(R, T(B, c, y, d), z, e) ->
      T(B, balance(B, T(R, T(B, a, w, b), x, c), y, d), z, e)
    B, EE, x, T(R, T(B, c, y, d), z, e) ->
      T(B, balance(B, T(R, E, x, c), y, d), z, e)
    B, T(R, a, w, T(B, b, x, c)), y, T(BB, d, z, e) ->
      T(B, a, w, balance(B, b, x, T(R, c, y, T(B, d, z, e))))
    B, T(R, a, w, T(B, b, x, c)), y, EE ->
      T(B, a, w, balance(B, b, x, T(R, c, y, E)))
    c, a, x, b -> T(c, a, x, b)
  }
}

type MinDel(a) {
  Min(a, Node(a))
  None
}

fn min_del(node) -> MinDel(a) {
  case node {
    T(R, E, x, E) -> Min(x, E)
    T(B, E, x, E) -> Min(x, EE)
    T(B, E, x, T(R, E, y, E)) -> Min(x, T(B, E, y, E))
    T(c, a, x, b) ->
      case min_del(a) {
        Min(x1, a1) -> Min(x1, rotate(c, a1, x, b))
        None -> None
      }
    _ -> None
  }
}

fn do_find(node, key, compare) {
  case node {
    T(_, l, k, r) ->
      case compare(key, k) {
        Lt -> do_find(l, key, compare)
        Gt -> do_find(r, key, compare)
        Eq -> Ok(k)
      }
    _ -> Error(Nil)
  }
}

fn do_fold(node, acc, fun) {
  case node {
    T(_, r, v, l) -> {
      let acc = do_fold(r, acc, fun)
      let acc = fun(acc, v)
      let acc = do_fold(l, acc, fun)
      acc
    }
    _ -> acc
  }
}

fn do_foldr(node, acc, fun) {
  case node {
    T(_, r, v, l) -> {
      let acc = do_foldr(l, acc, fun)
      let acc = fun(acc, v)
      let acc = do_foldr(r, acc, fun)
      acc
    }
    _ -> acc
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
    T(_, l, k, r) -> {
      let ls = do_draw(l, indent + 1, to_string)
      let ks = do_indent(to_string(k) <> "\n", indent)
      let rs = do_draw(r, indent + 1, to_string)
      ls <> ks <> rs
    }
    _ -> ""
  }
}
