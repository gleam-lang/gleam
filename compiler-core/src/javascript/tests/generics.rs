use crate::assert_ts_def;

#[test]
fn fn_generics_typescript() {
    assert_ts_def!(
        r#"pub fn indentity(a) -> a {
  a
}
"#,
    );
}

#[test]
fn record_generics_typescript() {
    assert_ts_def!(
        r#"pub type Animal(t) {
  Cat(type_: t)
  Dog(type_: t)
}

pub fn main() {
  Cat(type_: 6)
}
"#,
    );
}

#[test]
fn tuple_generics_typescript() {
    assert_ts_def!(
        r#"pub fn make_tuple(x: t) -> #(Int, t, Int) {
  #(0, x, 1)
}
"#,
    );
}

#[test]
fn externals_generics_typescript() {
    assert_ts_def!(
        r#"pub type Queue(a)

pub external fn new() -> Queue(a) = "queue" "new"

pub external fn length(Queue(a)) -> Int = "queue" "len"

pub external fn push(Queue(a), a) -> Queue(a) = "queue" "in"
"#,
    );
}

#[test]
fn result_typescript() {
    assert_ts_def!(
        r#"pub fn map(result, fun) {
            case result {
              Ok(a) -> Ok(fun(a))
              Error(e) -> Error(e)
            }
          }"#,
    );
}

#[test]
fn task_typescript() {
    assert_ts_def!(
        r#"pub type Promise(value)
    pub type Task(a) = fn() -> Promise(a)"#,
    );
}
