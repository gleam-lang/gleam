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
    return "Hello, Gleam!";
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
    true;
    return false;
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
    true && true;
    return false || false;
}"#
    );

}

// TODO add section to booke with equality operators

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
    1;
    2;
    -3;
    4001;
    0b00001111;
    0o17;
    0xF;
    return 1_000;
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
    1.5;
    2.0;
    -0.1;
    return 1.;
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
    1 + 1;
    5 - 1;
    Math.floor(5 / 2);
    3 * 3;
    5 % 2;
    2 > 1;
    2 < 1;
    2 >= 1;
    return 2 <= 1;
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
    1.0 + 1.4;
    5.0 - 1.5;
    5.0 / 2.0;
    3.0 * 3.1;
    2.0 > 1.0;
    2.0 < 1.0;
    2.0 >= 1.0;
    return 2.0 <= 1.0;
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
    let x = 1;
    let y = x;
    return x;
}"#
    );
}

#[test]
fn list_destructuring() {
    assert_js!(
        r#"
fn go(a) {
    let [] = a
    let [x, y] = a
    a
}"#,
r#"function go(a) {
    var gleam$tmp = a;
    if (!(gleam$tmp.length === 0)) throw new Error("Bad match")
    let [] = gleam$tmp;
    
    var gleam$tmp = a;
    if (!(gleam$tmp.length === 2)) throw new Error("Bad match")
    let [x, y] = gleam$tmp;
    
    return a;
}"#
    );

    assert_js!(
        r#"
fn go(a) {
    let [first, ..rest] = a
    a
}"#,
r#"function go(a) {
    var gleam$tmp = a;
    if (!(gleam$tmp.length >= 1)) throw new Error("Bad match")
    let [first, ...rest] = gleam$tmp;
    
    return a;
}"#
    );
}

#[test]
fn literal_matching() {
    assert_js!(
        r#"
fn go(a, b, c) {
    let 1 = a
    let 2.0 = b
    let "hello" = c
    a
}"#,
r#"function go(a, b, c) {
    var gleam$tmp = a;
    if (!(gleam$tmp === 1)) throw new Error("Bad match")
    let _ = gleam$tmp;
    
    var gleam$tmp = b;
    if (!(gleam$tmp === 2.0)) throw new Error("Bad match")
    let _ = gleam$tmp;
    
    var gleam$tmp = c;
    if (!(gleam$tmp === "hello")) throw new Error("Bad match")
    let _ = gleam$tmp;
    
    return a;
}"#
    );
}

#[test]
fn tuple_destructuring() {
    assert_js!(
        r#"
fn go(a, b) {
    let tuple(_, _ignore, x) = a
    let tuple(1, 2) = b
    // let tuple(tuple(1, 2) as x, 3) = c
    a
}"#,
r#"function go(a, b) {
    let [_, _, x] = a;
    var gleam$tmp = b;
    if (!(gleam$tmp[0] === 1 && gleam$tmp[1] === 2)) throw new Error("Bad match")
    let [_, _] = gleam$tmp;
    
    return a;
}"#
    );

}

// TODO write up about expression to tmp variable

// TODO look at purescript an buckle script but solve later.
// https://github.com/purescript/documentation/blob/master/language/Pattern-Matching.md

#[test]
fn expression_blocks() {
    assert_js!(
        r#"
fn go() {
    let value: Bool = {
        "Hello"
        42 + 12
        False
    } // => False
    value
}"#,
r#"function go() {
    let value = (
        "Hello",
        42 + 12,
        false
    );
    return value;
}"#
    );
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
    [];
    [1];
    [1, 2, 3];
    [1, 2, 3];
    return [1, ...x];
}"#
    );
}

#[test]
fn tuple_literals() {
    assert_js!(
        r#"
fn go() {
    let my_tuple = tuple("one", "two")
    my_tuple.0 // "one"
    my_tuple.1 // "two"

}"#,
r#"function go() {
    let my_tuple = ["one", "two"];
    my_tuple[0];
    return my_tuple[1];
}"#
    );
}

#[test]
#[ignore]
fn case_expressions() {
    assert_js!(
        r#"
fn go(a) {
case a, 1 + 2 {
    0, 1 -> "Zero"
    x, y if y > 4 -> "Four"
    _, _ -> "Other"
}
}"#,
r#"function go(a, b) {
    // Still needs a gleam name because maybe relying on outside value
return (function(gleam$sources) {
    if (gleam$sources[0] === 0 && gleam$sources[1] == 1) {
        var [_, _] = gleam$sources;
        return "Zero";
    };
    if (gleam$sources[1] >= 4) {
        var [x, y] = gleam$sources;
        return "Four";
    };

    if (true) {
        var [_, _] = gleam$sources;
        return "Other";
    }
}([a, 1 + 2]))
    
    // This approach require a comma section or it needs to reach up to work out what needs to happen with the last line
    var gleam$sources = [a, b], gleam$return

    if (gleam$sources[0] === 0) {
        var [_] = gleam$sources
        gleam$return = "Zero"
    } 


    return 
}"#
    );
}

#[test]
fn exported_functions(){
    assert_js!(
        r#"
pub fn add(x, y) {
    x + y
}"#,
r#"export function add(x, y) {
    return x + y;
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
    return f(f(x));
}

export function add_one(x) {
    return x + 1;
}

export function add_two(x) {
    return twice(add_one, x);
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
    return x + 1;
}

function add_two() {
    return add_one(add_one(1));
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
        return x + y;
    };
    return add(1, 2);
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
    return x + y;
}

function go() {
    let add_one = function(gleam@capture_variable) {
        return add(1, gleam@capture_variable);
    };
    return add_one(2);
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
r#"import * as rocket_ship from rocket_ship;

import * as foo from rocket_ship;"#
    );
}


#[test]
// How to we track to object keys, would we rather generate a function and call in order.
fn custom_type_with_named_fields() {
    assert_js!(
        r#"
type Cat{
    Cat(name: String, cuteness: Int)
}

// Does JS do clever ness with named args?
fn go() {
    Cat("Nubi", 1)
    Cat(2, name: "Nubi")
    Cat(cuteness: 3, name: "Nubi")
}
"#,
r#"function go() {
    {type: "Cat", name: "Nubi", cuteness: 1};
    {type: "Cat", name: "Nubi", cuteness: 2};
    return {type: "Cat", name: "Nubi", cuteness: 3};
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
    todo("I should do this");
}
"#,r#"function go() {
    throw Object.assign(new Error("I should do this"), {});
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
"#,r#"const start_year = 2101;

export const end_year = 2111;

export function is_before(year) {
    return year < start_year;
}"#
    );

    assert_js!(
        r#"
const a = "Hello"
const b = 1.2
// const c = []
const d = [1, 2]
const e = tuple("bob", "dug")
        "#,
        r#"const a = "Hello";

const b = 1.2;

const d = [1, 2];

const e = ["bob", "dug"];"#
    );
}

// TODO have Constant statements not be separated

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