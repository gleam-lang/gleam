use super::*;

use camino::Utf8Path;

fn fix(target: Option<Target>, src: &str) -> String {
    let (out, complete) = parse_fix_and_format(target, &src.into(), Utf8Path::new("test")).unwrap();
    assert!(complete);
    out
}

#[test]
fn empty() {
    assert_eq!(fix(None, ""), "\n")
}

#[test]
fn js() {
    assert_eq!(
        fix(
            None,
            r#"
pub external fn main() -> Int = "app.js" "main"
"#
        ),
        r#"@external(javascript, "app.js", "main")
pub fn main() -> Int
"#
    )
}

#[test]
fn mjs() {
    assert_eq!(
        fix(
            None,
            r#"
pub external fn main() -> Int = "app.mjs" "main"
"#
        ),
        r#"@external(javascript, "app.mjs", "main")
pub fn main() -> Int
"#
    )
}

#[test]
fn ts() {
    assert_eq!(
        fix(
            None,
            r#"
pub external fn main() -> Int = "app.ts" "main"
"#
        ),
        r#"@external(javascript, "app.ts", "main")
pub fn main() -> Int
"#
    )
}

#[test]
fn tsx() {
    assert_eq!(
        fix(
            None,
            r#"
pub external fn main() -> Int = "app.tsx" "main"
"#
        ),
        r#"@external(javascript, "app.tsx", "main")
pub fn main() -> Int
"#
    )
}

#[test]
fn jsx() {
    assert_eq!(
        fix(
            None,
            r#"
pub external fn main() -> Int = "app.jsx" "main"
"#
        ),
        r#"@external(javascript, "app.jsx", "main")
pub fn main() -> Int
"#
    )
}

#[test]
fn elixir() {
    assert_eq!(
        fix(
            None,
            r#"
pub external fn main() -> Int = "Elixir.App" "main"
"#
        ),
        r#"@external(erlang, "Elixir.App", "main")
pub fn main() -> Int
"#
    )
}

#[test]
fn subpath() {
    assert_eq!(
        fix(
            None,
            r#"
pub external fn main() -> Int = "one/two" "main"
"#
        ),
        r#"@external(javascript, "one/two", "main")
pub fn main() -> Int
"#
    )
}

#[test]
fn relative() {
    assert_eq!(
        fix(
            None,
            r#"
pub external fn main() -> Int = "./wobble" "main"
"#
        ),
        r#"@external(javascript, "./wobble", "main")
pub fn main() -> Int
"#
    )
}

#[test]
fn doc_comment() {
    assert_eq!(
        fix(
            None,
            r#"
/// Hello
pub external fn main() -> Int = "./wobble" "main"
"#
        ),
        r#"/// Hello
@external(javascript, "./wobble", "main")
pub fn main() -> Int
"#
    )
}

#[test]
fn comment() {
    assert_eq!(
        fix(
            None,
            r#"
// Hello
pub external fn main() -> Int = "./wobble" "main"
"#
        ),
        r#"// Hello
@external(javascript, "./wobble", "main")
pub fn main() -> Int
"#
    )
}

#[test]
fn if_javascript() {
    assert_eq!(
        fix(
            None,
            r#"
if javascript {
  pub external fn main() -> Int = "wobble" "main"
}
"#
        ),
        r#"@target(javascript)
@external(javascript, "wobble", "main")
pub fn main() -> Int
"#
    )
}

#[test]
fn if_erlang() {
    assert_eq!(
        fix(
            None,
            r#"
if erlang {
  pub external fn main() -> Int = "wobble" "main"
}
"#
        ),
        r#"@target(erlang)
@external(erlang, "wobble", "main")
pub fn main() -> Int
"#
    )
}

#[test]
fn if_both() {
    assert_eq!(
        fix(
            None,
            r#"
if erlang {
  pub external fn main() -> Int = "wobble" "main"
}

if javascript {
  pub external fn main() -> Int = "wobble" "main"
}
"#
        ),
        r#"@external(erlang, "wobble", "main")
@external(javascript, "wobble", "main")
pub fn main() -> Int
"#
    )
}

#[test]
fn parameters() {
    assert_eq!(
        fix(
            None,
            r#"
pub external fn main(Int, Float) -> Int = "app.js" "main"
"#
        ),
        r#"@external(javascript, "app.js", "main")
pub fn main(a: Int, b: Float) -> Int
"#
    )
}

#[test]
fn labels() {
    assert_eq!(
        fix(
            None,
            r#"
pub external fn main(wibble: Int, wobble: Float) -> Int = "app.js" "main"
"#
        ),
        r#"@external(javascript, "app.js", "main")
pub fn main(wibble wibble: Int, wobble wobble: Float) -> Int
"#
    )
}

#[test]
fn assume_erlang() {
    assert_eq!(
        fix(
            Some(Target::Erlang),
            r#"
pub external fn main(wibble: Int, wobble: Float) -> Int = "app" "main"
"#
        ),
        r#"@external(erlang, "app", "main")
pub fn main(wibble wibble: Int, wobble wobble: Float) -> Int
"#
    )
}

#[test]
fn assume_javascript() {
    assert_eq!(
        fix(
            Some(Target::Erlang),
            r#"
pub external fn main(wibble: Int, wobble: Float) -> Int = "app" "main"
"#
        ),
        r#"@external(erlang, "app", "main")
pub fn main(wibble wibble: Int, wobble wobble: Float) -> Int
"#
    )
}

#[test]
fn incompatible_returns() {
    assert_eq!(
        fix(
            None,
            r#"
if erlang {
  pub external fn main() -> List(Int) = "wobble" "main"
}

if javascript {
  pub external fn main() -> List(Float) = "wobble" "main"
}
"#
        ),
        r#"@target(erlang)
@external(erlang, "wobble", "main")
pub fn main() -> List(Int)

@target(javascript)
@external(javascript, "wobble", "main")
pub fn main() -> List(Float)
"#
    )
}

#[test]
fn incompatible_returns_qualified() {
    assert_eq!(
        fix(
            None,
            r#"
if erlang {
  pub external fn main() -> Int = "wobble" "main"
}

if javascript {
  pub external fn main() -> some.Int = "wobble" "main"
}
"#
        ),
        r#"@target(erlang)
@external(erlang, "wobble", "main")
pub fn main() -> Int

@target(javascript)
@external(javascript, "wobble", "main")
pub fn main() -> some.Int
"#
    )
}

#[test]
fn incompatible_arguments() {
    assert_eq!(
        fix(
            None,
            r#"
if erlang {
  pub external fn main(Int, List(Int)) -> Nil = "wobble" "main"
}

if javascript {
  pub external fn main(Int, List(Float)) -> Nil = "wobble" "main"
}
"#
        ),
        r#"@target(erlang)
@external(erlang, "wobble", "main")
pub fn main(a: Int, b: List(Int)) -> Nil

@target(javascript)
@external(javascript, "wobble", "main")
pub fn main(a: Int, b: List(Float)) -> Nil
"#
    )
}

#[test]
fn incompatible_arguments_tuple() {
    assert_eq!(
        fix(
            None,
            r#"
if erlang {
  pub external fn main(Int, #(Int, Int)) -> Nil = "wobble" "main"
}

if javascript {
  pub external fn main(Int, #(Int, Float)) -> Nil = "wobble" "main"
}
"#
        ),
        r#"@target(erlang)
@external(erlang, "wobble", "main")
pub fn main(a: Int, b: #(Int, Int)) -> Nil

@target(javascript)
@external(javascript, "wobble", "main")
pub fn main(a: Int, b: #(Int, Float)) -> Nil
"#
    )
}

#[test]
fn incompatible_arguments_fn() {
    assert_eq!(
        fix(
            None,
            r#"
if erlang {
  pub external fn main(Int, fn(Int, Int) -> Int) -> Nil = "wobble" "main"
}

if javascript {
  pub external fn main(Int, fn(Int, Float) -> Int) -> Nil = "wobble" "main"
}
"#
        ),
        r#"@target(erlang)
@external(erlang, "wobble", "main")
pub fn main(a: Int, b: fn(Int, Int) -> Int) -> Nil

@target(javascript)
@external(javascript, "wobble", "main")
pub fn main(a: Int, b: fn(Int, Float) -> Int) -> Nil
"#
    )
}

#[test]
fn incompatible_arguments_fn_length() {
    assert_eq!(
        fix(
            None,
            r#"
if erlang {
  pub external fn main(Int, fn(Int) -> Int) -> Nil = "wobble" "main"
}

if javascript {
  pub external fn main(Int, fn(Int, Float) -> Int) -> Nil = "wobble" "main"
}
"#
        ),
        r#"@target(erlang)
@external(erlang, "wobble", "main")
pub fn main(a: Int, b: fn(Int) -> Int) -> Nil

@target(javascript)
@external(javascript, "wobble", "main")
pub fn main(a: Int, b: fn(Int, Float) -> Int) -> Nil
"#
    )
}

#[test]
fn incompatible_arguments_fn_return() {
    assert_eq!(
        fix(
            None,
            r#"
if erlang {
  pub external fn main(Int, fn(Int) -> Int) -> Nil = "wobble" "main"
}

if javascript {
  pub external fn main(Int, fn(Int) -> Float) -> Nil = "wobble" "main"
}
"#
        ),
        r#"@target(erlang)
@external(erlang, "wobble", "main")
pub fn main(a: Int, b: fn(Int) -> Int) -> Nil

@target(javascript)
@external(javascript, "wobble", "main")
pub fn main(a: Int, b: fn(Int) -> Float) -> Nil
"#
    )
}

#[test]
fn incompatible_arguments_tuple_length() {
    assert_eq!(
        fix(
            None,
            r#"
if erlang {
  pub external fn main(Int, #(Int)) -> Nil = "wobble" "main"
}

if javascript {
  pub external fn main(Int, #(Int, Float)) -> Nil = "wobble" "main"
}
"#
        ),
        r#"@target(erlang)
@external(erlang, "wobble", "main")
pub fn main(a: Int, b: #(Int)) -> Nil

@target(javascript)
@external(javascript, "wobble", "main")
pub fn main(a: Int, b: #(Int, Float)) -> Nil
"#
    )
}

#[test]
fn ambiguous_external() {
    let src = r#"pub external fn main(wibble: Int, wobble: Float) -> Int =
  "app" "main"
"#;
    let (out, complete) = parse_fix_and_format(None, &src.into(), Utf8Path::new("test")).unwrap();
    assert!(!complete);
    assert_eq!(out, src)
}
