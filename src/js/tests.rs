use super::*;

macro_rules! assert_js {
    ($src:expr, $erl:expr $(,)?) => {
        // println!("\n\n\n{}\n", $src);
        let (mut ast, _) = crate::parse::parse_module($src).expect("syntax error");
        ast.name = vec!["the_app".to_string()];
        let ast =
            crate::type_::infer_module(&mut 0, ast, &std::collections::HashMap::new(), &mut vec![])
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