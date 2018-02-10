module Maybe

export Maybe(..), just?/1, nothing?/1, map/2, flatten/1, flat_map/2, unwrap/2


type Maybe(x)
  = Just(x)
  | Nothing

let just?(maybe) =
  match maybe
  | Just(_) => True
  | Nothing => False

test "just?/1" =
  just?(Just(1)) |> Assert.true
  just?(Nothing) |> Assert.false

let nothing?(maybe) =
  match maybe
  | Just(_) => False
  | Nothing => True

test "nothing?/1" =
  nothing?(Just(1)) |> Assert.false
  nothing?(Nothing) |> Assert.true

let map(maybe, fun) =
  match maybe
  | Just(x) => fun(x)
  | Nothing => Nothing

test "map/2" =
  map(Just(1), |x| x + 1) |> Assert.equal(_, Just(2))
  map(Nothing, |x| x + 1) |> Assert.equal(Nothing)

let flatten(maybe) =
  maybe
    |> unwrap(_, Nothing)

test "flatten/1" =
  flatten(Just(Just(1))) |> Assert.equal(Just(1))
  flatten(Just(Nothing)) |> Assert.equal(Nothing)
  flatten(Nothing) |> Assert.equal(Nothing)

let flat_map(maybe, fun) =
  maybe
    |> map(_, fun)
    |> flatten

test "flat_map/2" =
  flat_map(Nothing, |x| Just(x + 1)) |> Assert.equal(Nothing)
  flat_map(Just(1), |x| Just(x + 1)) |> Assert.equal(Just(2))
  flat_map(Just(1), |_| Nothing) |> Assert.equal(Nothing)

let unwrap(maybe, fallback) =
  match maybe
  | Just(v) => v
  | Nothing => fallback

test "unwrap/2" =
  unwrap(Just(1), 50) |> Assert.equal(1)
  unwrap(Nothing, 50) |> Assert.equal(50)
