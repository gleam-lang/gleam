use super::*;
use crate::build::{Origin, Module};
use crate::type_;

macro_rules! assert_js {
    ($src:expr, $erl:expr $(,)?) => {
        // println!("\n\n\n{}\n", $src);
        let (mut ast, _) = crate::parse::parse_module($src).expect("syntax error");
        ast.name = vec!["the_app".to_string()];
        let mut modules = std::collections::HashMap::new();
        // Probably a nicer way to do types values accessors
        modules.insert(
            "rocket_ship".to_string(), 
            (Origin::Src, type_::Module { name: vec!["rocket_ship".to_string()], types: std::collections::HashMap::new(), values: std::collections::HashMap::new(), accessors: std::collections::HashMap::new()})
        );
        let ast =
            crate::type_::infer_module(&mut 0, ast, &modules, &mut vec![])
                .expect("should successfully infer");
        let mut output = String::new();
        let line_numbers = LineNumbers::new($src);
        module(&ast, &line_numbers, &mut output).unwrap();
        assert_eq!(($src, output), ($src, $erl.to_string()));
    };
}

#[test]
#[ignore]
fn string_literals() {
    assert_js!(
        r#"
fn go() {
    "Hello, Gleam!"
}
"#,
r#"function go() {
    return "Hello, Gleam!"
}"#
    );
}

// \" escape sequence works in js.

#[test]
#[ignore]
fn boolean_literals() {
    assert_js!(
        r#"
fn go() {
    True
}
"#,
r#"function go() {
    return true
}"#
    );
}


#[test]
fn number_literals() {
    assert_js!(
        r#"
fn go() {
    1
    2
    -3
    4001
    0b00001111
    0o17
    0xF
    1_000
}
"#,
r#"function go() {
    1
    2
    -3
    4001
    0b00001111
    0o17
    0xF
    1_000
}"#
    );
    // NOTE js also supports _ in numbers

    assert_js!(
        r#"
fn go() {
    1.5
    2.0
    -0.1
    1.
}
"#,
r#"function go() {
    1.5
    2.0
    -0.1
    1.
}"#
    );
}

#[test]
fn number_operators() {
    assert_js!(
        r#"
fn go() {
    1 + 1 // => 2
    5 - 1 // => 4
    5 / 2 // => 2
    3 * 3 // => 9
    5 % 2 // => 1

    2 > 1  // => True
    2 < 1  // => False
    2 >= 1 // => True
    2 <= 1 // => False
}
"#,
r#"function go() {
    1 + 1
    5 - 1
    Math.floor(5 / 2)
    3 * 3
    5 % 2
    2 > 1
    2 < 1
    2 >= 1
    2 <= 1
}"#
    );

    assert_js!(
        r#"
fn go() {
    1.0 +. 1.4 // => 2.4
    5.0 -. 1.5 // => 3.5
    5.0 /. 2.0 // => 2.5
    3.0 *. 3.1 // => 9.3
    
    2.0 >. 1.0  // => True
    2.0 <. 1.0  // => False
    2.0 >=. 1.0 // => True
    2.0 <=. 1.0 // => False
}
"#,
r#"function go() {
    1.0 + 1.4
    5.0 - 1.5
    5.0 / 2.0
    3.0 * 3.1
    2.0 > 1.0
    2.0 < 1.0
    2.0 >= 1.0
    2.0 <= 1.0
}"#
    );
    
}

#[test]
fn let_bindings() {
    assert_js!(
        r#"
fn go() {
    let x = 1
    let y = x
    // let x = 2
    x
}"#,
r#"function go() {
    let x = 1
    let y = x
    x
}"#
    );

//     assert_js!(
//         r#"
// fn go() {
//     let x = { 
//         1 + 2
//         3 + 1
//     }
//     x
// }"#,
// r#"function go() {
//     let x = 1
//     let y = x
//     x
// }"#
//     );
// TODO handle let x = { 1 + 2}


}

#[test]
fn list_literals() {
    assert_js!(
        r#"
fn go(x) {
    []
    [1]
    [1, 2, 3]
    [1, ..[2, 3]]
    // TODO are improper lists possible
    // [[], ..2]
    [1, ..x]
    // Do lists with multiple Tails make sense? JS supports them.
    // [1, ..[2], ..[2]]
    // TODO handle blocks, single element blocks removed by the formatter
    // [1, ..{ 
    //     "hello"
    //     [2, 3] 
    // }]
}"#,
r#"function go(x) {
    []
    [1]
    [1, 2, 3]
    [1, 2, 3]
    [1, ...x]
}"#
    );
}

#[test]
fn tuple_literals() {
    assert_js!(
        r#"
fn go() {
    let my_tuple = tuple("one", "two")
    my_tuple.0   // "one"
    my_tuple.1 // "two"

}"#,
r#"function go() {
    let my_tuple = ["one", "two"]
    my_tuple[0]
    my_tuple[1]
}"#
    );
}

// TODO case expressions, need an answer for how to do blocks.

#[test]
fn exported_functions(){
    assert_js!(
        r#"
pub fn add(x, y) {
    x + y
}"#,
r#"export function add(x, y) {
    x + y
}"#
    );
}

#[test]
fn calling_functions(){
    assert_js!(
        r#"
pub fn twice(f: fn(t) -> t, x: t) -> t {
    f(f(x))
}

pub fn add_one(x: Int) -> Int {
    x + 1
}

pub fn add_two(x: Int) -> Int {
    twice(add_one, x)
}"#,
r#"export function twice(f, x) {
    f(f(x))
}

export function add_one(x) {
    x + 1
}

export function add_two(x) {
    twice(add_one, x)
}"#
    );
}


// Note pipes are syntactic sugar and nothing special is needed to render them.

#[test]
fn using_pipes(){
    assert_js!(
        r#"
fn add_one(x: Int) -> Int {
    x + 1
}
        
fn add_two() {
    1
    |> add_one()
    |> add_one()
}"#,
r#"function add_one(x) {
    x + 1
}

function add_two() {
    add_one(add_one(1))
}"#
    );
}


#[test]
fn anonymous_functions() {
    assert_js!(
        r#"
fn go() {
    let add = fn(x, y) { x + y }

    add(1, 2)
}"#,
r#"function go() {
    let add = function(x, y) {
        x + y
    }
    add(1, 2)
}"#
    );
}

// Note function capturing is just syntax sugar, and all printing done with functions code.
#[test]
fn function_capturing() {
    assert_js!(
        r#"
fn add(x, y) {
    x + y
}

fn go() {
    let add_one = add(1, _)

    add_one(2)
}"#,
r#"function add(x, y) {
    x + y
}

function go() {
    let add_one = function(gleam@capture_variable) {
        add(1, gleam@capture_variable)
    }
    add_one(2)
}"#
    );
}

// TODO @ is not an acceptable variable charachter in JS, need a better name, 
// potentially variable is a type rather than string with name.

#[test]
fn importing_a_module() {
    assert_js!(
        r#"
// uncommenting the unknown import crashes the tests
// Note import does nothing in erlang land.
import rocket_ship
import rocket_ship as foo
// import rocket_ship.{launch}
"#,
r#"import * as rocket_ship from rocket_ship

import * as foo from rocket_ship"#
    );
}


#[test]
#[ignore]
// How to we track to object keys, would we rather generate a function and call in order.
fn custom_types() {
    assert_js!(
        r#"
type Cat{
    Cat(name: String, cuteness: Int)
}

fn go() {
    Cat(name: "Nubi", cuteness: 2001)
}
"#,
r#"function go() {
    {gleam_record: "Cat", }
}"#
    );
}