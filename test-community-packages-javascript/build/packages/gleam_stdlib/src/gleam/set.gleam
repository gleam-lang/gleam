import gleam/list
import gleam/map.{Map}
import gleam/result

if erlang {
  // A list is used as the map value as an empty list has the smallest
  // representation in Erlang's binary format
  type Token =
    List(Nil)

  const token = []
}

if javascript {
  type Token =
    Nil

  const token = Nil
}

/// A set is a collection of unique members of the same type.
///
/// It is implemented using the `gleam/map` module, so inserts and lookups have
/// logarithmic time complexity.
///
pub opaque type Set(member) {
  Set(map: Map(member, Token))
}

/// Creates a new empty set.
///
pub fn new() -> Set(member) {
  Set(map.new())
}

/// Gets the number of members in a set.
///
/// This function runs in constant time.
///
/// ## Examples
///
/// ```gleam
/// > new()
/// > |> insert(1)
/// > |> insert(2)
/// > |> size
/// 2
/// ```
///
pub fn size(set: Set(member)) -> Int {
  map.size(set.map)
}

/// Inserts an member into the set.
///
/// This function runs in logarithmic time.
///
/// ## Examples
///
/// ```gleam
/// > new()
/// > |> insert(1)
/// > |> insert(2)
/// > |> size
/// 2
/// ```
///
pub fn insert(into set: Set(member), this member: member) -> Set(member) {
  Set(map: map.insert(set.map, member, token))
}

/// Checks whether a set contains a given member.
///
/// This function runs in logarithmic time.
///
/// ## Examples
///
/// ```gleam
/// > new()
/// > |> insert(2)
/// > |> contains(2)
/// True
/// ```
///
/// ```gleam
/// > new()
/// > |> insert(2)
/// > |> contains(1)
/// False
/// ```
///
pub fn contains(in set: Set(member), this member: member) -> Bool {
  set.map
  |> map.get(member)
  |> result.is_ok
}

/// Removes a member from a set. If the set does not contain the member then
/// the set is returned unchanged.
///
/// This function runs in logarithmic time.
///
/// ## Examples
///
/// ```gleam
/// > new()
/// > |> insert(2)
/// > |> delete(2)
/// > |> contains(1)
/// False
/// ```
///
pub fn delete(from set: Set(member), this member: member) -> Set(member) {
  Set(map: map.delete(set.map, member))
}

/// Converts the set into a list of the contained members.
///
/// The list has no specific ordering, any unintentional ordering may change in
/// future versions of Gleam or Erlang.
///
/// This function runs in linear time.
///
/// ## Examples
///
/// ```gleam
/// > new() |> insert(2) |> to_list
/// [2]
/// ```
///
pub fn to_list(set: Set(member)) -> List(member) {
  map.keys(set.map)
}

/// Creates a new set of the members in a given list.
///
/// This function runs in loglinear time.
///
/// ## Examples
///
/// ```gleam
/// > import gleam/list
/// > [1, 1, 2, 4, 3, 2] |> from_list |> to_list |> list.sort
/// [1, 3, 3, 4]
/// ```
///
pub fn from_list(members: List(member)) -> Set(member) {
  let map =
    list.fold(
      over: members,
      from: map.new(),
      with: fn(m, k) { map.insert(m, k, token) },
    )
  Set(map)
}

/// Combines all entries into a single value by calling a given function on each
/// one.
///
/// Sets are not ordered so the values are not returned in any specific order.
/// Do not write code that relies on the order entries are used by this
/// function as it may change in later versions of Gleam or Erlang.
///
/// # Examples
///
/// ```gleam
/// > from_list([1, 3, 9])
/// > |> fold(0, fn(member, accumulator) { accumulator + member })
/// 13
/// ```
///
pub fn fold(
  over set: Set(member),
  from initial: acc,
  with reducer: fn(acc, member) -> acc,
) -> acc {
  map.fold(over: set.map, from: initial, with: fn(a, k, _) { reducer(a, k) })
}

/// Creates a new set from an existing set, minus any members that a given
/// function returns `False` for.
///
/// This function runs in loglinear time.
///
/// ## Examples
///
/// ```gleam
/// > import gleam/int
/// > from_list([1, 4, 6, 3, 675, 44, 67])
/// > |> filter(for: int.is_even)
/// > |> to_list
/// [4, 6, 44]
/// ```
///
pub fn filter(
  in set: Set(member),
  for property: fn(member) -> Bool,
) -> Set(member) {
  Set(map.filter(in: set.map, for: fn(m, _) { property(m) }))
}

pub fn drop(from set: Set(member), drop disallowed: List(member)) -> Set(member) {
  list.fold(over: disallowed, from: set, with: delete)
}

/// Creates a new map from a given map, only including any members which are in
/// a given list.
///
/// This function runs in loglinear time.
///
/// ## Examples
///
/// ```gleam
/// > from_list([1, 2, 3])
/// > |> take([1, 3, 5])
/// > |> to_list
/// [1, 3]
/// ```
///
pub fn take(from set: Set(member), keeping desired: List(member)) -> Set(member) {
  Set(map.take(from: set.map, keeping: desired))
}

fn order(first: Set(member), second: Set(member)) -> #(Set(member), Set(member)) {
  case map.size(first.map) > map.size(second.map) {
    True -> #(first, second)
    False -> #(second, first)
  }
}

/// Creates a new set that contains all members of both given sets.
///
/// This function runs in loglinear time.
///
/// ## Examples
///
/// ```gleam
/// > union(from_list([1, 2]), from_list([2, 3])) |> to_list
/// [1, 2, 3]
/// ```
///
pub fn union(of first: Set(member), and second: Set(member)) -> Set(member) {
  let #(larger, smaller) = order(first, second)
  fold(over: smaller, from: larger, with: insert)
}

/// Creates a new set that contains members that are present in both given sets.
///
/// This function runs in loglinear time.
///
/// ## Examples
///
/// ```gleam
/// > intersection(from_list([1, 2]), from_list([2, 3])) |> to_list
/// [2]
/// ```
///
pub fn intersection(
  of first: Set(member),
  and second: Set(member),
) -> Set(member) {
  let #(larger, smaller) = order(first, second)
  take(from: larger, keeping: to_list(smaller))
}
