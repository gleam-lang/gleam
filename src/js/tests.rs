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
fn string_literals() {
    assert_js!(
        r#"
fn go() {
    "Hello, Gleam!"
}
"#,
r#"function go() {
    "Hello, Gleam!"
}"#
    );
}

// \" escape sequence works in js.

#[test]
fn boolean_literals() {
    assert_js!(
        r#"
fn go() {
    True
    False
}
"#,
r#"function go() {
    true
    false
}"#
    );
}

#[test]
fn boolean_operators() {
    assert_js!(
        r#"
fn go() {
    True && True
    False || False
}
"#,
r#"function go() {
    true && true
    false || false
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

// I don't think result needs a special section as it's generated as any other record.

#[test]
#[ignore]
fn try_syntax_sugar(){
    assert_js!(
        r#"
fn parse() {
    Ok(5)
}
        
fn go() {
    try x = parse()
    Ok(Nil)
}
"#,r#"function parse() {
    Ok(5)
}

function go() {
}"#
    );
}

// TODO why is piping handled in the AST before but try falls through??

// TODO assert similar question to try.


#[test]
#[ignore]
fn todo_throws_error(){
    assert_js!(
        r#"
fn go() {
    todo
}
"#,r#"function go() {
    throw Object.assign(new Error("This has not yet been implemented"), {})
}"#
    );
    assert_js!(
        r#"
fn go() {
    todo("I should do this")
}
"#,r#"function go() {
    throw Object.assign(new Error("I should do this"), {})
}"#
    );
}


#[test]
fn constant_statements(){
    assert_js!(
        r#"
const start_year = 2101
pub const end_year = 2111

pub fn is_before(year: Int) -> Bool {
    year < start_year
}

// pub fn is_during(year: Int) -> Bool {
//     start_year <= year && year <= end_year
// }
"#,r#"const start_year = 2101

export const end_year = 2111

export function is_before(year) {
    year < start_year
}"#
    );
}

#[test]
#[ignore]
fn external_functions(){
    assert_js!(
        r#"
external type Element
external fn query_selector(String) -> Element = "document" "querySelector"
// TODO test named external

fn go (selector: String) {
    query_selector(selector)
}
"#,r#"function query_selector(arg0) {
    document.querySelector(arg0)
}

function go(selector) {
    query_selector(selector)
}"#
    );
}