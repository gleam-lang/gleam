module Maybe

export Maybe(..), is_just/1, is_nothing/1, map/2, flatten/1, flat_map/2,
       unwrap/2

type Maybe(x)
  = Just(x)
  | Nothing

fn is_just(maybe) =
  case maybe
  | Just(_) => True
  | Nothing => False

test is_just() =
  is_just(Just(1)) |> Assert.true
  is_just(Nothing) |> Assert.false

fn is_nothing(maybe) =
  case maybe
  | Just(_) => False
  | Nothing => True

test is_nothing() =
  is_nothing(Just(1)) |> Assert.false
  is_nothing(Nothing) |> Assert.true

fn map(maybe, fun) =
  case maybe
  | Just(x) => fun(x)
  | Nothing => Nothing

test map() =
  map(Just(1), |x| x + 1) |> Assert.equal(_, Just(2))
  map(Nothing, |x| x + 1) |> Assert.equal(Nothing)

fn flatten(maybe) =
  maybe
    |> unwrap(_, Nothing)

test flatten() =
  flatten(Just(Just(1))) |> Assert.equal(Just(1))
  flatten(Just(Nothing)) |> Assert.equal(Nothing)
  flatten(Nothing) |> Assert.equal(Nothing)

fn flat_map(maybe, fun) =
  maybe
    |> map(_, fun)
    |> flatten

test flat_map() =
  flat_map(Nothing, |x| Just(x + 1)) |> Assert.equal(Nothing)
  flat_map(Just(1), |x| Just(x + 1)) |> Assert.equal(Just(2))
  flat_map(Just(1), |_| Nothing) |> Assert.equal(Nothing)

fn unwrap(maybe, fallback) =
  case maybe
  | Just(v) => v
  | Nothing => fallback

test unwrap() =
  unwrap(Just(1), 50) |> Assert.equal(1)
  unwrap(Nothing, 50) |> Assert.equal(50)
