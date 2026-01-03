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
}

#[test]
fn clause_guards_1() {
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
}

#[test]
fn clause_guards_2() {
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
}

#[test]
fn clause_guards_3() {
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
}

#[test]
fn clause_guards_4() {
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
}

#[test]
fn clause_guards_5() {
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
}

#[test]
fn clause_guards_6() {
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
}

#[test]
fn clause_guards_7() {
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
}

#[test]
fn clause_guards_8() {
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
}

#[test]
fn clause_guards_9() {
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
}

#[test]
fn clause_guards_10() {
    assert_erl!(
        r#"
pub fn main() {
  let x = 0.123
  case x {
    _ if x == 3.14 -> 1
    _ -> 0
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
    _ -> 0
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
    _ -> 0
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
    _ -> 0
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
    _ -> 0
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
    _ -> 0
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
fn clause_guards32() {
    assert_erl!(
        r#"
pub fn main() {
  let wibble = "wobble"
  case wibble {
    x if x == "wob" <> "ble" -> 1
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
}

#[test]
fn constants_in_guards1() {
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
}

#[test]
fn only_guards1() {
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
}

#[test]
fn only_guards2() {
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
}

#[test]
fn only_guards3() {
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

#[test]
fn field_access() {
    assert_erl!(
        r#"
        pub type Person {
          Person(username: String, name: String, age: Int)
        }
        
        pub fn main() {
          let given_name = "jack"
          let raiden = Person("raiden", "jack", 31)
        
          case given_name {
            name if name == raiden.name -> "It's jack"
            _ -> "It's not jack"
          }
        }
        "#
    )
}

#[test]
fn nested_record_access() {
    assert_erl!(
        r#"
pub type A {
  A(b: B)
}

pub type B {
  B(c: C)
}

pub type C {
  C(d: Bool)
}

pub fn a(a: A) {
  case a {
    _ if a.b.c.d -> 1
    _ -> 0
  }
}
"#
    );
}

#[test]
fn module_string_access() {
    assert_erl!(
        (
            "package",
            "hero",
            r#"
              pub const ironman = "Tony Stark"
            "#
        ),
        r#"
          import hero
          pub fn main() {
            let name = "Tony Stark"
            case name {
              n if n == hero.ironman -> True
              _ -> False
            }
          }
        "#
    );
}

#[test]
fn module_list_access() {
    assert_erl!(
        (
            "package",
            "hero",
            r#"
              pub const heroes = ["Tony Stark", "Bruce Wayne"]
            "#
        ),
        r#"
          import hero
          pub fn main() {
            let names = ["Tony Stark", "Bruce Wayne"]
            case names {
              n if n == hero.heroes -> True
              _ -> False
            }
          }
        "#
    );
}

#[test]
fn module_tuple_access() {
    assert_erl!(
        (
            "package",
            "hero",
            r#"
              pub const hero = #("ironman", "Tony Stark")
            "#
        ),
        r#"
          import hero
          pub fn main() {
            let name = "Tony Stark"
            case name {
              n if n == hero.hero.1 -> True
              _ -> False
            }
          }
        "#
    );
}

#[test]
fn module_access() {
    assert_erl!(
        (
            "package",
            "hero",
            r#"
              pub type Hero {
                Hero(name: String)
              }
              pub const ironman = Hero("Tony Stark")
            "#
        ),
        r#"
          import hero
          pub fn main() {
            let name = "Tony Stark"
            case name {
              n if n == hero.ironman.name -> True
              _ -> False
            }
          }
        "#
    );
}

#[test]
fn module_nested_access() {
    assert_erl!(
        (
            "package",
            "hero",
            r#"
              pub type Person {
                Person(name: String)
              }
              pub type Hero {
                Hero(secret_identity: Person)
              }
              const bruce = Person("Bruce Wayne")
              pub const batman = Hero(bruce)
            "#
        ),
        r#"
          import hero
          pub fn main() {
            let name = "Bruce Wayne"
            case name {
              n if n == hero.batman.secret_identity.name -> True
              _ -> False
            }
          }
        "#
    );
}
