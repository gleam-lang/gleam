import any
import expect

pub external type Map(key, value);

pub external fn new() -> Map(key, value))
  = 'maps' 'new'

test new {
  new()
    |> size
    |> expect:equal(_, 0)
}

pub external fn size(Map(_, _)) -> Int
  = 'maps' 'size'

test size {
  []
    |> from_list
    |> size
    |> expect:equal(_, 0)

  [{1, 1}]
    |> from_list
    |> size
    |> expect:equal(_, 1)

  [{"", 1.0}, {"", 2.0}]
    |> from_list
    |> size
    |> expect:equal(_, 2)
}

external fn is_key(key, Map(key, _)) -> Bool
  = 'maps' 'is_key'

pub fn has_key(map, key) {
  is_key(key, map)
}

test has_key {
  []
    |> from_list
    |> has_key(_, 1)
    |> expect:false

  [{1, 0}]
    |> from_list
    |> has_key(_, 1)
    |> expect:true

  [{4, 0}, {1, 0}]
    |> from_list
    |> has_key(_, 1)
    |> expect:true

  [{4, 0}, {1, 0}]
    |> from_list
    |> has_key(_, 0)
    |> expect:false
}

pub fn from_record(record: {r}) -> Map(Atom, any:Any) {
  any:unsafeCoerce(record)
}

test from_record {
  map = from_record({ name = "Jane" })

  map
    |> size
    |> expect:equal(_, 1)

  map
    |> expect:equal(_, from_list([{'name', "Jane"}]))
}

pub external fn to_list(Map(key, value)) -> List(Tuple(key, value))
  = 'maps' 'to_list'

pub external fn from_list(List(Tuple(key, value))) -> Map(key, value)
  = 'maps' 'from_list'

test from_list {
  proplist = [{4, 0}, {1, 0}]
  map = from_list(proplist)

  map
    |> size
    |> expect:equal(_, 2)

  map
    |> to_list
    |> expect:equal(_, proplist)
}
