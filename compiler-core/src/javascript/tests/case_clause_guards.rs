use crate::assert_js;

#[test]
fn referencing_pattern_var() {
    assert_js!(
        r#"pub fn main(xs) {
  case xs {
    #(x) if x -> 1
    _ -> 0
  }
}
"#,
    );
}

#[test]
fn rebound_var() {
    assert_js!(
        r#"pub fn main() {
  let x = False
  let x = True
  case x {
    _ if x -> 1
    _ -> 0
  }
}
"#,
    );
}

#[test]
fn bitarray_with_var() {
    assert_js!(
        r#"pub fn main() {
  case 5 {
    z if <<z>> == <<z>> -> Nil
    _ -> Nil
  }
}
"#,
    )
}

// https://github.com/gleam-lang/gleam/issues/3004
#[test]
fn keyword_var() {
    assert_js!(
        r#"
pub const function = 5
pub const do = 10
pub fn main() {
  let class = 5
  let while = 10
  let var = 7
  case var {
    _ if class == while -> True
    _ if [class] == [5] -> True
    function if #(function) == #(5) -> False
    _ if do == function -> True
    while if while > 5 -> False
    class -> False
  }
}
"#,
    );
}

#[test]
fn operator_wrapping_right() {
    assert_js!(
        r#"pub fn main(xs, y: Bool, z: Bool) {
  case xs {
    #(x) if x == { y == z } -> 1
    _ -> 0
  }
}
"#,
    );
}

#[test]
fn operator_wrapping_left() {
    assert_js!(
        r#"pub fn main(xs, y: Bool, z: Bool) {
  case xs {
    #(x) if { x == y } == z -> 1
    _ -> 0
  }
}
"#,
    );
}

#[test]
fn eq_scalar() {
    assert_js!(
        r#"pub fn main(xs, y: Int) {
  case xs {
    #(x) if x == y -> 1
    _ -> 0
  }
}
"#,
    );
}

#[test]
fn not_eq_scalar() {
    assert_js!(
        r#"pub fn main(xs, y: Int) {
  case xs {
    #(x) if x != y -> 1
    _ -> 0
  }
}
"#,
    );
}

#[test]
fn tuple_index() {
    assert_js!(
        r#"pub fn main(x, xs: #(Bool, Bool, Bool)) {
  case x {
    _ if xs.2 -> 1
    _ -> 0
  }
}
"#,
    );
}

#[test]
fn not_eq_complex() {
    assert_js!(
        r#"pub fn main(xs, y) {
  case xs {
    #(x) if xs != y -> x
    _ -> 0
  }
}
"#,
    );
}

#[test]
fn eq_complex() {
    assert_js!(
        r#"pub fn main(xs, y) {
  case xs {
    #(x) if xs == y -> x
    _ -> 0
  }
}
"#,
    );
}

#[test]
fn constant() {
    assert_js!(
        r#"pub fn main(xs) {
  case xs {
    #(x) if x == 1 -> x
    _ -> 0
  }
}
"#,
    );
}

#[test]
fn alternative_patterns() {
    assert_js!(
        r#"pub fn main(xs) {
  case xs {
    1 | 2 -> 0
    _ -> 1
  }
}
"#,
    );
}

#[test]
fn alternative_patterns_list() {
    assert_js!(
        r#"pub fn main(xs) -> Int {
  case xs {
    [1] | [1, 2] -> 0
    _ -> 1
  }
}
"#,
    );
}

#[test]
fn alternative_patterns_assignment() {
    assert_js!(
        r#"pub fn main(xs) -> Int {
  case xs {
    [x] | [_, x] -> x
    _ -> 1
  }
}
"#,
    );
}

#[test]
fn alternative_patterns_guard() {
    assert_js!(
        r#"pub fn main(xs) -> Int {
  case xs {
    [x] | [_, x] if x == 1 -> x
    _ -> 0
  }
}
"#,
    );
}

#[test]
fn field_access() {
    assert_js!(
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
    assert_js!(
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
    assert_js!(
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
    assert_js!(
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
    assert_js!(
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
    assert_js!(
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
fn module_access_submodule() {
    assert_js!(
        (
            "package",
            "hero/submodule",
            r#"
              pub type Hero {
                Hero(name: String)
              }
              pub const ironman = Hero("Tony Stark")
            "#
        ),
        r#"
          import hero/submodule
          pub fn main() {
            let name = "Tony Stark"
            case name {
              n if n == submodule.ironman.name -> True
              _ -> False
            }
          }
        "#
    );
}

#[test]
fn module_access_aliased() {
    assert_js!(
        (
            "package",
            "hero/submodule",
            r#"
              pub type Hero {
                Hero(name: String)
              }
              pub const ironman = Hero("Tony Stark")
            "#
        ),
        r#"
          import hero/submodule as myhero
          pub fn main() {
            let name = "Tony Stark"
            case name {
              n if n == myhero.ironman.name -> True
              _ -> False
            }
          }
        "#
    );
}

#[test]
fn module_nested_access() {
    assert_js!(
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

#[test]
fn not() {
    assert_js!(
        r#"pub fn main(x, y) {
  case x {
    _ if !y -> 0
    _ -> 1
  }
}
"#,
    );
}

#[test]
fn not_two() {
    assert_js!(
        r#"pub fn main(x, y) {
  case x {
    _ if !y && !x -> 0
    _ -> 1
  }
}
"#,
    );
}

#[test]
fn custom_type_constructor_imported_and_aliased() {
    assert_js!(
        ("package", "other_module", "pub type T { A }"),
        r#"import other_module.{A as B}
fn func() {
  case B {
    x if x == B -> True
    _ -> False
  }
}
"#,
    );
}

#[test]
fn imported_aliased_ok() {
    assert_js!(
        r#"import gleam.{Ok as Y}
pub type X {
  Ok
}
fn func() {
  case Y {
    y if y == Y -> True
    _ -> False
  }
}
"#,
    );
}

#[test]
fn imported_ok() {
    assert_js!(
        r#"import gleam
pub type X {
  Ok
}
fn func(x) {
  case gleam.Ok {
    _ if [] == [ gleam.Ok ] -> True
    _ -> False
  }
}
"#,
    );
}

// Variant of https://github.com/lpil/decode/pull/6
#[test]
fn constructor_function_in_guard() {
    assert_js!(
        r#"fn func(x) {
    case [] {
        _ if [] == [ Ok ] -> True
        _ -> False
    }
}
    "#,
    );
}
