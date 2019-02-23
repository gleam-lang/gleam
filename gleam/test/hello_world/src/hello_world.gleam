// A binary tree with leaves carrying an integer
pub enum Tree =
  | Leaf(Int)
  | Node(Tree, Tree)

pub fn any(tree, predicate) {
  case tree {
  | Leaf(i) -> predicate(i)
  | Node(left, right) -> any(left, predicate) || any(right, predicate)
  }
}

pub fn has_even_leaf(tree) {
  any(tree, fn(i) {
    i % 2 == 0
  })
}

pub fn person(name) {
  { name = name }
}

pub fn put_age(record, age) {
  { record | age = age }
}

pub fn person_with_age(name, age) {
  put_age(person(name), age)
}

pub fn get_age(record) {
  record.age
}

pub fn multiline() {
  "hello
  \"
world"
}

import bool

pub fn not(b) {
  bool:not(b)
  // [1, 2, 3, 4.0, 5]
  // 1 + 2.0
}
