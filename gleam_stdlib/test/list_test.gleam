import expect
import list

pub fn length_test() {
  list:length([]) |> expect:equal(_, 0)
  list:length([1]) |> expect:equal(_, 1)
  list:length([1, 1]) |> expect:equal(_, 2)
  list:length([1, 1, 1]) |> expect:equal(_, 3)
}

pub fn reverse_test() {
  list:reverse([]) |> expect:equal(_, [])
  list:reverse([1, 2, 3, 4, 5]) |> expect:equal(_, [5, 4, 3, 2, 1])
}

pub fn is_empty_test() {
  list:is_empty([]) |> expect:true
  list:is_empty([1]) |> expect:false
}

pub fn contains_test() {
  list:contains([0, 4, 5, 1], 1) |> expect:true
  list:contains([0, 4, 5, 7], 1) |> expect:false
  list:contains([], 1) |> expect:false
}

pub fn head_test() {
  list:head([0, 4, 5, 7])
    |> expect:equal(_, Ok(0))

  list:head([])
    |> expect:is_error
}

pub fn tail_test() {
  list:tail([0, 4, 5, 7])
    |> expect:equal(_, Ok([4, 5, 7]))

  list:tail([0])
    |> expect:equal(_, Ok([]))

  list:tail([])
    |> expect:is_error
}

pub fn filter_test() {
  []
    |> list:filter(_, fn(_) { True })
    |> expect:equal(_, [])

  [0, 4, 5, 7, 3]
    |> list:filter(_, fn(_) { True })
    |> expect:equal(_, [0, 4, 5, 7, 3])

  [0, 4, 5, 7, 3]
    |> list:filter(_, fn(x) { x > 4 })
    |> expect:equal(_, [5, 7])

  [0, 4, 5, 7, 3]
    |> list:filter(_, fn(x) { x < 4 })
    |> expect:equal(_, [0, 3])
}

pub fn map_test() {
  []
    |> list:map(_, fn(x) { x * 2 })
    |> expect:equal(_, [])

  [0, 4, 5, 7, 3]
    |> list:map(_, fn(x) { x * 2 })
    |> expect:equal(_, [0, 8, 10, 14, 6])
}

pub fn traverse_test() {
  let fun = fn(x) {
    case x == 6 || x == 5 || x == 4 {
    | True -> Ok(x * 2)
    | False -> Error(x)
    }
  }

  [5, 6, 5, 6]
    |> list:traverse(_, fun)
    |> expect:equal(_, Ok([10, 12, 10, 12]))

  [4, 6, 5, 7, 3]
    |> list:traverse(_, fun)
    |> expect:equal(_, Error(7))
}

pub fn drop_test() {
  []
    |> list:drop(_, 5)
    |> expect:equal(_, [])

  [1, 2, 3, 4, 5, 6, 7, 8]
    |> list:drop(_, 5)
    |> expect:equal(_, [6, 7, 8])
}

pub fn take_test() {
  []
    |> list:take(_, 5)
    |> expect:equal(_, [])
  [1, 2, 3, 4, 5, 6, 7, 8]
    |> list:take(_, 5)
    |> expect:equal(_, [1, 2, 3, 4, 5])
}

pub fn new_test() {
  list:new() |> expect:equal(_, [])
}

pub fn append_test() {
  list:append([1], [2, 3])
    |> expect:equal(_, [1, 2, 3])
}

pub fn flatten_test() {
  list:flatten([])
    |> expect:equal(_, [])

  list:flatten([[]])
    |> expect:equal(_, [])

  list:flatten([[], [], []])
    |> expect:equal(_, [])

  list:flatten([[1, 2], [], [3, 4]])
    |> expect:equal(_, [1, 2, 3, 4])
}

pub fn fold_test() {
  [1, 2, 3]
    |> list:fold(_, [], fn(x, acc) { [x | acc] })
    |> expect:equal(_, [3, 2, 1])
}

pub fn fold_right_test() {
  [1, 2, 3]
    |> list:fold_right(_, [], fn(x, acc) { [x | acc] })
    |> expect:equal(_, [1, 2, 3])
}

pub fn find_test() {
  let f = fn(x) {
    case x {
    | 2 -> Ok(4)
    | _ -> Error(0)
    }
  }

  [1, 2, 3]
  |> list:find(_, f)
  |> expect:equal(_, Ok(4))

  [1, 3, 2]
  |> list:find(_, f)
  |> expect:equal(_, Ok(4))

  [1, 3]
  |> list:find(_, f)
  |> expect:is_error
}

pub fn all_test() {
  list:all([1,2,3,4,5], fn(x) { x > 0 })
  |> expect:equal(_, True)

  list:all([1,2,3,4,5], fn(x) { x < 0 })
  |> expect:equal(_, False)

  list:all([], fn(_) { False })
  |> expect:equal(_, True)
}
