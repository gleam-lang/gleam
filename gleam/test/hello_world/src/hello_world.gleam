// A binary tree with leaves carrying an integer
enum Tree =
  | Leaf(Int)
  | Node(Tree, Tree)

fn any(tree, predicate) {
  case tree {
  | Leaf(i) -> predicate(i)
  | Node(left, right) ->
      case any(left, predicate) {
      | True -> True
      | False -> any(right, predicate)
      }
  }
}
