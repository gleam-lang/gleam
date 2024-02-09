// Based on "Purely Functional Data Structures" by Okasaki (1998)

import gleam/order.{Gt, Order}

type T(a) {
  E
  T(a, List(T(a)))
}

pub opaque type Heap(a) {
  Heap(root: T(a), compare: fn(a, a) -> Order)
}

pub fn new(compare: fn(a, a) -> Order) -> Heap(a) {
  Heap(E, compare)
}

pub fn insert(heap: Heap(a), key: a) -> Heap(a) {
  Heap(merge(T(key, []), heap.root, heap.compare), heap.compare)
}

pub fn find_min(heap: Heap(a)) -> Result(a, Nil) {
  case heap.root {
    T(x, _) -> Ok(x)
    E -> Error(Nil)
  }
}

pub fn delete_min(heap: Heap(a)) -> Result(#(a, Heap(a)), Nil) {
  case heap.root {
    T(x, xs) -> Ok(#(x, Heap(merge_pairs(xs, heap.compare), heap.compare)))
    E -> Error(Nil)
  }
}

fn merge(x: T(a), y: T(a), compare: fn(a, a) -> Order) -> T(a) {
  case x, y {
    x, E -> x
    E, y -> y
    T(xk, xs), T(yk, ys) ->
      case compare(xk, yk) {
        Gt -> T(yk, [x, ..ys])
        _ -> T(xk, [y, ..xs])
      }
  }
}

fn merge_pairs(l: List(T(a)), compare: fn(a, a) -> Order) -> T(a) {
  case l {
    [] -> E
    [h] -> h
    [h1, h2, ..hs] ->
      merge(merge(h1, h2, compare), merge_pairs(hs, compare), compare)
  }
}
