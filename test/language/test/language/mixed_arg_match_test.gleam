type Cat {
  Cat(String, cuteness: Int)
}

type NestedCat {
  NestedCat(Cat, String, cuteness: Int)
}

pub fn matching_second_labelled_arg_as_first_test() {
  let Cat(cuteness: y, ..) = Cat("fluffy", 10)
  assert y == 10
}

pub fn matching_both_args_on_position_test() {
  let Cat(x, y) = Cat("fluffy", 10)
  assert #(x, y) == #("fluffy", 10)
}

pub fn matching_second_labelled_arg_as_second_test() {
  let Cat(x, cuteness: y) = Cat("fluffy", 10)
  assert #(x, y) == #("fluffy", 10)
}

pub fn nested_custom_types_test() {
  let NestedCat(Cat(x, cuteness: y), cuteness: y2, ..) =
    NestedCat(Cat("fluffy", 10), "gleamy", 100)
  assert #(x, y, y2) == #("fluffy", 10, 100)
}
