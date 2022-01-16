use crate::assert_erl;

#[test]
fn clause_guards() {
    // Clause guards
    assert_erl!(
        r#"
pub fn main(args) {
  case args {
    x if x == args -> 1
    _ -> 0
  }
}
"#
    );

    assert_erl!(
        r#"
pub fn main(args) {
  case args {
    x if {x != x} == {args == args} -> 1
    _ -> 0
  }
}
"#
    );

    assert_erl!(
        r#"
pub fn main(args) {
  case args {
    x if x && x || x == x && x -> 1
    _ -> 0
  }
}
"#
    );

    assert_erl!(
        r#"
pub fn main() {
  case 1, 0 {
    x, y if x > y -> 1
    _, _ -> 0
  }
}
"#
    );

    assert_erl!(
        r#"
pub fn main() {
  case 1, 0 {
    x, y if x >= y -> 1
    _, _ -> 0
  }
}
"#
    );

    assert_erl!(
        r#"
pub fn main() {
  case 1, 0 {
    x, y if x < y -> 1
    _, _ -> 0
  }
}
"#
    );

    assert_erl!(
        r#"
pub fn main() {
  case 1, 0 {
    x, y if x <= y -> 1
    _, _ -> 0
  }
}
"#
    );

    assert_erl!(
        r#"
pub fn main() {
  case 1.0, 0.1 {
    x, y if x >. y -> 1
    _, _ -> 0
  }
}
"#
    );

    assert_erl!(
        r#"
pub fn main() {
  case 1.0, 0.1 {
    x, y if x >=. y -> 1
    _, _ -> 0
  }
}
"#
    );

    assert_erl!(
        r#"
pub fn main() {
  let x = 0.123
  case x {
    99.9854 -> 1
    _ -> 0
  }
}
"#
    );

    assert_erl!(
        r#"
pub fn main() {
  let x = 0.123
  case x {
    _ if x == 3.14 -> 1
  }
}
"#
    );
}

#[test]
fn clause_guards20() {
    assert_erl!(
        r#"
pub fn main() {
  let x = 0.123
  case x {
    _ if 0.123 <. x -> 1
  }
}
"#
    );
}

#[test]
fn clause_guards21() {
    assert_erl!(
        r#"
pub fn main(x) {
  case x {
    _ if x == [1, 2, 3] -> 1
  }
}
"#
    );
}

#[test]
fn clause_guards22() {
    assert_erl!(
        r#"
pub fn main() {
  let x = 0
  case x {
    0 -> 1
    _ -> 0
  }
}
"#
    );
}

#[test]
fn clause_guards23() {
    // Tuple literals in guards

    assert_erl!(
        r#"
pub fn main() {
  let x = #(1, 2, 3)
  case x {
    _ if x == #(1, 2, 3) -> 1
    _ -> 0
  }
}
"#
    );
}

#[test]
fn clause_guards24() {
    assert_erl!(
        r#"
pub fn main() {
  let x = #(1, 2, 3)
  case x {
    _ if x == #(1, 2, 3) -> 1
    _ if x == #(2, 3, 4) -> 2
    _ -> 0
  }
}
"#
    );
}

#[test]
fn clause_guards25() {
    // Int literals in guards

    assert_erl!(
        r#"
pub fn main() {
  let x = 0
  case x {
    _ if x == 0 -> 1
  }
}
"#
    );
}

#[test]
fn clause_guards26() {
    assert_erl!(
        r#"
pub fn main() {
  let x = 0
  case x {
    _ if 0 < x -> 1
  }
}
"#
    );
}

#[test]
fn clause_guards27() {
    // String literals in guards

    assert_erl!(
        r#"
pub fn main() {
  case "test" {
    x if x == "test" -> 1
  }
}
"#
    );
}

#[test]
fn clause_guards28() {
    // Record literals in guards

    assert_erl!(
        r#"
    type Test { Test(x: Int, y: Float) }
    pub fn main() {
      let x = Test(1, 3.0)
      case x {
        _ if x == Test(1, 1.0) -> 1
        _ if x == Test(y: 2.0, x: 2) -> 2
        _ if x != Test(2, 3.0) -> 2
        _ -> 0
      }
    }
"#
    );
}

#[test]
fn clause_guards29() {
    // Float vars in guards

    assert_erl!(
        r#"
pub fn main() {
  case 0.1, 1.0 {
    x, y if x <. y -> 1
    _, _ -> 0
  }
}
"#
    );
}

#[test]
fn clause_guards30() {
    assert_erl!(
        r#"
pub fn main() {
  case 0.1, 1.0 {
    x, y if x <=. y -> 1
    _, _ -> 0
  }
}
"#
    );
}

#[test]
fn clause_guards31() {
    assert_erl!(
        r#"
pub fn main(args) {
  case args {
    [x] | [x, _] if x -> 1
    _ -> 0
  }
}
"#
    );
}

#[test]
fn constants_in_guards() {
    assert_erl!(
        r#"
pub const string_value = "constant value"
pub const float_value = 3.14
pub const int_value = 42
pub const tuple_value = #(1, 2.0, "3")
pub const list_value = [1, 2, 3]

pub fn main(arg) {
  let _ = list_value
  case arg {
    #(w, x, y, z) if w == tuple_value && x == string_value && y >. float_value && z == int_value -> 1
    _ -> 0
  }
}
"#
    );

    assert_erl!(
        r#"
pub const list = [1, 2, 3]

pub fn main(arg) {
  case arg {
    _ if arg == list -> 1
    _ -> 0
  }
}
"#
    );
}

#[test]
fn only_guards() {
    assert_erl!(
        r#"
pub const string_value = "constant value"

pub fn main(arg) {
  case arg {
    _ if arg == string_value -> 1
    _ -> 0
  }
}
"#
    );

    assert_erl!(
        r#"
pub const bits = <<1, "ok":utf8, 3, 4:50>>

pub fn main(arg) {
  case arg {
    _ if arg == bits -> 1
    _ -> 0
  }
}
"#
    );

    assert_erl!(
        r#"
pub const constant = #(1, 2.0)

pub fn main(arg) {
  case arg {
    _ if arg == constant -> 1
    _ -> 0
  }
}
"#
    );

    assert_erl!(
        r#"
pub const float_value = 3.14

pub fn main(arg) {
  case arg {
    _ if arg >. float_value -> 1
    _ -> 0
  }
}
"#
    );
}
