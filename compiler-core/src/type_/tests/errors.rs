use super::*;

macro_rules! assert_module_error {
    ($src:expr, $error:expr $(,)?) => {
        let (mut ast, _) = crate::parse::parse_module($src).expect("syntax error");
        ast.name = vec!["my_module".to_string()];
        let mut modules = im::HashMap::new();
        let ids = UniqueIdGenerator::new();
        // DUPE: preludeinsertion
        // TODO: Currently we do this here and also in the tests. It would be better
        // to have one place where we create all this required state for use in each
        // place.
        let _ = modules.insert("gleam".to_string(), build_prelude(&ids));
        let ast = infer_module(
            Target::Erlang,
            &ids,
            ast,
            Origin::Src,
            "thepackage",
            &modules,
            &mut vec![],
        )
        .expect_err("should infer an error");
        assert_eq!(($src, sort_options($error)), ($src, sort_options(ast)));
    };

    ($src:expr) => {
        let (ast, _) = crate::parse::parse_module($src).expect("syntax error");
        let mut modules = im::HashMap::new();
        let ids = UniqueIdGenerator::new();
        // DUPE: preludeinsertion
        // TODO: Currently we do this here and also in the tests. It would be better
        // to have one place where we create all this required state for use in each
        // place.
        let _ = modules.insert("gleam".to_string(), build_prelude(&ids));
        let error = infer_module(
            Target::Erlang,
            &ids,
            ast,
            Origin::Src,
            "thepackage",
            &modules,
            &mut vec![],
        )
        .expect_err("should infer an error");
        let error = crate::error::Error::Type {
            src: $src.to_string(),
            path: PathBuf::from("/src/one/two.gleam"),
            error,
        };
        let output = error.pretty_string();
        insta::assert_snapshot!(insta::internals::AutoName, output, $src);
    };
}

macro_rules! assert_error {
    ($src:expr, $error:expr $(,)?) => {
        let ast = crate::parse::parse_expression_sequence($src).expect("syntax error");
        let ids = UniqueIdGenerator::new();
        let mut modules = im::HashMap::new();
        // DUPE: preludeinsertion
        // TODO: Currently we do this here and also in the tests. It would be better
        // to have one place where we create all this required state for use in each
        // place.
        let _ = modules.insert("gleam".to_string(), build_prelude(&ids));
        println!("new assert_error test: {}", modules.len());
        let result = ExprTyper::new(&mut Environment::new(
            ids,
            &["somemod".to_string()],
            &modules,
            &mut vec![],
        ))
        .infer(ast)
        .expect_err("should infer an error");
        assert_eq!(($src, sort_options($error)), ($src, sort_options(result)),);
    };

    ($src:expr) => {
        use std::path::PathBuf;
        let ast = crate::parse::parse_expression_sequence($src).expect("syntax error");
        let ids = UniqueIdGenerator::new();
        let mut modules = im::HashMap::new();
        // DUPE: preludeinsertion
        // TODO: Currently we do this here and also in the tests. It would be better
        // to have one place where we create all this required state for use in each
        // place.
        let _ = modules.insert("gleam".to_string(), build_prelude(&ids));
        println!("new assert_error test: {}", modules.len());
        let error = ExprTyper::new(&mut Environment::new(
            ids,
            &["somemod".to_string()],
            &modules,
            &mut vec![],
        ))
        .infer(ast)
        .expect_err("should infer an error");
        let error = crate::error::Error::Type {
            src: $src.to_string(),
            path: PathBuf::from("/src/one/two.gleam"),
            error,
        };
        let output = error.pretty_string();
        insta::assert_snapshot!(insta::internals::AutoName, output, $src);
    };
}

#[test]
fn bit_string_invalid_type() {
    assert_module_error!(
        "fn x() { \"test\" }

fn main() {
    let a = <<1:size(x())>>
    a
}"
    );
}

#[test]
fn bit_strings2() {
    assert_error!("let <<x:utf8>> = <<1>> x");
}

#[test]
fn bit_strings3() {
    assert_error!("let <<x:utf16>> = <<1>> x");
}

#[test]
fn bit_strings4() {
    assert_error!("let <<x:utf32>> = <<1>> x");
}

#[test]
fn bit_string() {
    assert_error!("case <<1>> { <<2.0, a>> -> 1 }");
}

#[test]
fn bit_string_float() {
    assert_error!("case <<1>> { <<a:float>> if a > 1 -> 1 }");
}

#[test]
fn bit_string_binary() {
    assert_error!("case <<1>> { <<a:binary>> if a > 1 -> 1 }");
}

#[test]
fn bit_string_guard() {
    assert_error!("case <<1>> { <<a:utf16_codepoint>> if a == \"test\" -> 1 }");
}

#[test]
fn bit_string_segment_nosize() {
    assert_error!("case <<1>> { <<_:binary, _:binary>> -> 1 }");
}

#[test]
fn bit_string_segment_nosize2() {
    assert_error!("case <<1>> { <<_:bit_string, _:binary>> -> 1 }");
}

#[test]
fn bit_string_segment_nosize3() {
    assert_error!("case <<1>> { <<_:binary, _:bit_string>> -> 1 }");
}

#[test]
fn bit_string_segment_conflicting_options_int() {
    assert_error!("let x = <<1:int-binary>> x");
}

#[test]
fn bit_string_segment_conflicting_options_bit_string() {
    assert_error!("case <<1>> { <<1:bit_string-binary>> -> 1 }");
}

#[test]
fn bit_string_segment_conflicting_signedness1() {
    assert_error!("let x = <<1:signed-unsigned>> x");
}

#[test]
fn bit_string_segment_conflicting_signedness2() {
    assert_error!("case <<1>> { <<1:unsigned-signed>> -> 1 }");
}

#[test]
fn bit_string_segment_conflicting_endianness1() {
    assert_error!("let x = <<1:big-little>> x");
}

#[test]
fn bit_string_segment_conflicting_endianness2() {
    assert_error!("case <<1>> { <<1:native-big>> -> 1 }");
}

#[test]
fn bit_string_segment_size() {
    assert_error!("let x = <<1:8-size(5)>> x");
}

#[test]
fn bit_string_segment_size2() {
    assert_error!("case <<1>> { <<1:size(2)-size(8)>> -> a }");
}

#[test]
fn bit_string_segment_unit_unit() {
    assert_error!("let x = <<1:unit(2)-unit(5)>> x");
}

#[test]
fn bit_string_segment_type_does_not_allow_unit_codepoint_utf8() {
    assert_error!("let x = <<1:utf8_codepoint-unit(5)>> x");
}

#[test]
fn bit_string_segment_type_does_not_allow_unit_codepoint_utf16() {
    assert_error!("let x = <<1:utf16_codepoint-unit(5)>> x");
}

#[test]
fn bit_string_segment_type_does_not_allow_unit_codepoint_utf32() {
    assert_error!("case <<1>> { <<1:utf32_codepoint-unit(2)>> -> a }");
}

#[test]
fn bit_string_segment_type_does_not_allow_unit_codepoint_utf8_2() {
    assert_error!("let x = <<1:utf8_codepoint-size(5)>> x");
}

#[test]
fn bit_string_segment_type_does_not_allow_unit_codepoint_utf16_2() {
    assert_error!("let x = <<1:utf16_codepoint-size(5)>> x");
}

#[test]
fn bit_string_segment_type_does_not_allow_unit_codepoint_utf32_2() {
    assert_error!("case <<1>> { <<1:utf32_codepoint-size(5)>> -> a }");
}

#[test]
fn bit_string_segment_type_does_not_allow_unit_utf8_2() {
    assert_error!("let x = <<1:utf8-unit(5)>> x");
}

#[test]
fn bit_string_segment_type_does_not_allow_unit_utf16() {
    assert_error!("let x = <<1:utf16-unit(5)>> x");
}

#[test]
fn bit_string_segment_type_does_not_allow_unit_utf32() {
    assert_error!("case <<1>> { <<1:utf32-unit(2)>> -> a }");
}

#[test]
fn bit_string_segment_type_does_not_allow_size_utf8() {
    assert_error!("let x = <<1:utf8-size(5)>> x");
}

#[test]
fn bit_string_segment_type_does_not_allow_size_utf16() {
    assert_error!("let x = <<1:utf16-size(5)>> x");
}

#[test]
fn bit_string_segment_type_does_not_allow_size_utf32() {
    assert_error!("case <<1>> { <<1:utf32-size(5)>> -> a }");
}

#[test]
fn bit_string_segment_unit_no_size() {
    assert_error!("let x = <<1:unit(5)>> x");
}

#[test]
fn bit_string_size_not_int() {
    assert_error!("let x = <<1:size(\"1\")>> x");
}

#[test]
fn bit_string_size_not_int_variable() {
    assert_error!("let a = 2.0 case <<1>> { <<1:size(a)>> -> a }");
}

#[test]
fn bit_string_float_size() {
    // float given size
    assert_error!("let x = <<1:8-float>> x");
}

#[test]
fn bit_string_binary_option_in_value() {
    // using binary in value
    assert_error!("let x = <<<<1:1>>:binary>> x");
}

#[test]
fn add_int_float() {
    assert_error!("1 + 1.0");
}

#[test]
fn add_f_int_float() {
    assert_error!("1 +. 1.0");
}

#[test]
fn int_eq_float() {
    assert_error!("1 == 1.0");
}

#[test]
fn int_gt_float() {
    assert_error!("1 > 1.0");
}

#[test]
fn float_gtf_int() {
    assert_error!("1.0 >. 1");
}

#[test]
fn fn0_eq_fn1() {
    assert_error!("fn() { 1 } == fn(x) { x + 1 }");
}

#[test]
fn unknown_variable() {
    assert_error!("x");
}

#[test]
fn unknown_module() {
    assert_module_error!("import xpto");
}

#[test]
fn unknown_variable_2() {
    assert_error!("case 1 { x -> 1 1 -> x }");
}

#[test]
fn unknown_variable_3() {
    assert_error!("let add = fn(x, y) { x + y } 1 |> add(unknown)");
}

#[test]
fn incorrect_arity_error() {
    assert_error!("let id = fn(x) { x } id()");
}

#[test]
fn incorrect_arity_error_2() {
    assert_error!("let id = fn(x) { x } id(1, 2)");
}

#[test]
fn case_clause_mismatch() {
    assert_error!("case 1 { a -> 1 b -> 2.0 }");
}

#[test]
fn case_subject_pattern_unify() {
    assert_error!("case 1.0 { 1 -> 1 }");
}

#[test]
fn case_subject_pattern_unify_2() {
    assert_error!("case 1 { 1.0 -> 1 }");
}

#[test]
fn case_operator_unify_situation() {
    assert_error!("case 1, 2.0 { a, b -> a + b }");
}

#[test]
fn case_could_not_unify() {
    assert_error!("case 1, 2.0 { a, b -> a 1, 2 -> 0 }");
}

#[test]
fn assigned_function_annotation() {
    assert_error!("let f = fn(x: Int) { x } f(1.0)");
}

#[test]
fn function_return_annotation() {
    assert_error!("fn() -> Int { 2.0 }");
}

#[test]
fn function_arg_and_return_annotation() {
    assert_error!("fn(x: Int) -> Float { x }");
}

#[test]
fn function_return_annotation_mismatch_with_try() {
    assert_error!(
        "fn() -> Result(Nil, Nil) {
            let a = 1
            try _ = Error(1)
            // comments
            // comments
            // comments
            // comments
            // comments
            // comments
            // comments
            // comments
            // comments
            // comments
            Ok(Nil)
        }"
    );
}

#[test]
fn function_return_annotation_mismatch_with_try_nested() {
    assert_error!(
        "fn() -> Result(Nil, Nil) {
          try _ = {
            try _ = {
                try _ = Error(1)
                Ok(Nil)
            }
            Ok(Nil)
          }
          Ok(Nil)
        }"
    );
}

// https://github.com/gleam-lang/gleam/issues/1378
#[test]
fn function_return_annotation_mismatch_with_pipe() {
    assert_module_error!(
        "pub fn main() -> String {
            1
            |> add_two
         }
          
         fn add_two(i: Int) -> Int {
            i + 2
         }"
    );
}

#[test]
fn variable_annotation_with_try() {
    assert_error!(
        "let x: Result(Nil, Nil) = {
            let a_var = 1
            try _ = Error(1)
            Ok(Nil)
        }"
    );
}

#[test]
fn pipe_mismatch_error() {
    assert_module_error!(
        "pub fn main() -> String {
            Orange
            |> eat_veggie
         }
          
         type Fruit{ Orange }
         type Veg{ Lettuce }
          
         fn eat_veggie(v: Veg) -> String {
            \"Ok\"
         }"
    );
}

#[test]
fn case_tuple_guard() {
    assert_error!("case #(1, 2, 3) { x if x == #(1, 1.0) -> 1 }");
}

#[test]
fn case_list_guard() {
    assert_error!("case [1] { x if x == [1, 2.0] -> 1 }");
}

#[test]
fn case_tuple_guard_2() {
    assert_error!("case #(1, 2) { x if x == #(1, 1.0) -> 1 }");
}

#[test]
fn case_int_tuple_guard() {
    assert_error!("case 1 { x if x == #() -> 1 }");
}

#[test]
fn wrong_number_of_subjects() {
    assert_error!("case 1 { _, _ -> 1 }");
}

#[test]
fn recursive_var() {
    assert_error!("let id = fn(x) { x(x) } 1");
}

#[test]
fn true_fn() {
    assert_error!("let True(x) = 1 x");
}

#[test]
fn ok_2_args() {
    assert_error!("let Ok(1, x) = 1 x");
}

#[test]
fn access_int() {
    assert_error!("let x = 1 x.whatever");
}

#[test]
fn tuple_2_3() {
    assert_error!("#(1, 2) == #(1, 2, 3)");
}

#[test]
fn tuple_int_float() {
    assert_error!("#(1.0, 2, 3) == #(1, 2, 3)");
}

#[test]
fn tuple_int() {
    assert_error!("let #(a, b) = 1 a");
}

#[test]
fn int_float_list() {
    assert_error!("[1.0] == [1]");
}

#[test]
fn guard_int_float_eq_vars() {
    assert_error!("let x = 1 let y = 1.0 case x { _ if x == y -> 1 }");
}

#[test]
fn guard_float_int_eq_vars() {
    assert_error!("let x = 1.0 let y = 1 case x { _ if x == y -> 1 }");
}

#[test]
fn guard_if_float() {
    assert_error!("let x = 1.0 case x { _ if x -> 1 }");
}

#[test]
fn case() {
    assert_error!("case #(1, 1.0) { #(x, _) | #(_, x) -> 1 }");
}

#[test]
fn case2() {
    assert_error!("case [3.33], 1 { x, y if x > y -> 1 }");
}

#[test]
fn case3() {
    assert_error!("case 1, 2.22, \"three\" { x, _, y if x > y -> 1 }");
}

#[test]
fn case4() {
    assert_error!("case [3.33], 1 { x, y if x >= y -> 1 }");
}

#[test]
fn case5() {
    assert_error!("case 1, 2.22, \"three\" { x, _, y if x >= y -> 1 }");
}

#[test]
fn case6() {
    assert_error!("case [3.33], 1 { x, y if x < y -> 1 }");
}

#[test]
fn case7() {
    assert_error!("case 1, 2.22, \"three\" { x, _, y if x < y -> 1 }");
}

#[test]
fn case8() {
    assert_error!("case [3.33], 1 { x, y if x <= y -> 1 }");
}

#[test]
fn case9() {
    assert_error!("case 1, 2.22, \"three\" { x, _, y if x <= y -> 1 }");
}

#[test]
fn case10() {
    assert_error!("case [3], 1.1 { x, y if x >. y -> 1 }");
}

#[test]
fn case11() {
    assert_error!("case 2.22, 1, \"three\" { x, _, y if x >. y -> 1 }");
}

#[test]
fn case12() {
    assert_error!("case [3], 1.1 { x, y if x >=. y -> 1 }");
}

#[test]
fn case13() {
    assert_error!("case 2.22, 1, \"three\" { x, _, y if x >=. y -> 1 }");
}

#[test]
fn case14() {
    assert_error!("case [3], 1.1 { x, y if x <. y -> 1 }");
}

#[test]
fn case15() {
    assert_error!("case 2.22, 1, \"three\" { x, _, y if x <. y -> 1 }");
}

#[test]
fn case16() {
    assert_error!("case [3], 1.1 { x, y if x <=. y -> 1 }");
}

#[test]
fn case17() {
    assert_error!("case 2.22, 1, \"three\" { x, _, y if x <=. y -> 1 }");
}

#[test]
fn case18() {
    assert_error!("case 1 { x if x == \"x\" -> 1 }");
}

#[test]
fn case19() {
    assert_error!("case [1] { [x] | x -> 1 }");
}

#[test]
fn case20() {
    assert_error!("case [1] { [x] | [] as x -> 1 }");
}

#[test]
fn extra_var_inalternative() {
    assert_error!("case [1] { [x] | [x, y] -> 1 }");
}

#[test]
fn extra_var_inalternative2() {
    assert_error!("case #(1, 2) { #(1, y) | #(x, y) -> 1 }");
}

#[test]
fn extra_var_inalternative3() {
    assert_error!("let x = 1 case #(1, 2) { #(1, y) | #(x, y) -> 1 }");
}

#[test]
fn tuple_arity() {
    // https://github.com/gleam-lang/gleam/issues/714
    assert_error!("case #(1, 2) { #(1, _, _, _) -> 1 }");
}

#[test]
fn duplicate_vars() {
    assert_error!("case #(1, 2) { #(x, x) -> 1 }");
}

#[test]
fn duplicate_vars_2() {
    assert_error!("case [3.33], 1 { x, x if x > x -> 1 }");
}

#[test]
fn duplicate_vars_3() {
    assert_error!("case [1, 2, 3] { [x, x, y] -> 1 }");
}

#[test]
fn tuple_index_out_of_bounds() {
    assert_error!("#(0, 1).2");
}

#[test]
fn tuple_index_not_a_tuple() {
    assert_error!("Nil.2");
}

#[test]
fn tuple_index_not_a_tuple_unbound() {
    assert_error!("fn(a) { a.2 }");
}

#[test]
fn unknown_accessed_type() {
    assert_error!("fn(a) { a.field }");
}

#[test]
fn unknown_field() {
    assert_error!("fn(a: a) { a.field }");
}

#[test]
fn inconsistent_try_1() {
    assert_error!("try x = Error(1) try y = Error(1.) Ok(x)");
}

#[test]
fn inconsistent_try_2() {
    assert_error!("try x = Error(1) Error(1.)");
}

#[test]
fn inconsistent_try_3() {
    assert_error!("try x = Error(1) 1");
}

#[test]
fn inconsistent_try_4() {
    assert_error!("try y = Error(1) try z = Error(1.) Ok(1)");
}

#[test]
fn inconsistent_try_5() {
    assert_error!(r#"try x = Error(1) Error("Not this one") Error("This one")"#);
}

#[test]
fn field_not_in_all_variants() {
    assert_module_error!(
        "
pub type Person {
    Teacher(name: String, age: Int, title: String)
    Student(name: String, age: Int)
}
pub fn get_title(person: Person) { person.title }"
    );
}

#[test]
fn field_not_in_any_variant() {
    assert_module_error!(
        "
pub type Person {
    Teacher(name: String, age: Int, title: String)
    Student(name: String, age: Int)
}
pub fn get_height(person: Person) { person.height }"
    );
}

#[test]
fn field_type_different_between_variants() {
    assert_module_error!(
        "
pub type Shape {
    Square(x: Int, y: Int)
    Rectangle(x: String, y: String)
}
pub fn get_x(shape: Shape) { shape.x }
pub fn get_y(shape: Shape) { shape.y }"
    );
}

#[test]
fn accessor_multiple_variants_multiple_positions() {
    // We cannot access fields on custom types with multiple variants where they are in different positions e.g. 2nd and 3rd
    assert_module_error!(
        "
pub type Person {
    Teacher(name: String, title: String, age: Int)
    Student(name: String, age: Int)
}
pub fn get_name(person: Person) { person.name }
pub fn get_age(person: Person) { person.age }"
    );

    // We cannot access fields on custom types with multiple variants where they are in different positions e.g. 1st and 3rd
    assert_module_error!(
        "
pub type Person {
    Teacher(title: String, age: Int, name: String)
    Student(name: String, age: Int)
}
pub fn get_name(person: Person) { person.name }
pub fn get_age(person: Person) { person.age }"
    );
}

#[test]
fn module_could_not_unify() {
    assert_module_error!("fn go() { 1 + 2.0 }");
}

#[test]
fn module_could_not_unify2() {
    assert_module_error!("fn go() { 1 + 2.0 }");
}

#[test]
fn module_could_not_unify3() {
    assert_module_error!(
        "
fn id(x: a, y: a) { x }
pub fn x() { id(1, 1.0) }"
    );
}

#[test]
fn module_could_not_unify4() {
    assert_module_error!(
        "
fn bar() -> Int {
    5
}

fn run(one: fn() -> String) {
    one()
}

fn demo() {
    run(bar)
}"
    );
}

#[test]
fn module_could_not_unify5() {
    assert_module_error!(
        "
fn bar(x: Int) -> Int {
    x * 5
}

fn run(one: fn(String) -> Int) {
    one(\"one.\")
}

fn demo() {
    run(bar)
}"
    );
}

#[test]
fn module_could_not_unify6() {
    assert_module_error!("fn main() { let x: String = 5 x }");
}

#[test]
fn module_could_not_unify7() {
    assert_module_error!("fn main() { assert 5: Int = \"\" 5 }");
}

#[test]
fn module_could_not_unify8() {
    assert_module_error!("fn main() { let x: #(x, x) = #(5, 5.0) x }");
}

#[test]
fn module_could_not_unify9() {
    assert_module_error!("fn main() { let [1, 2, ..x]: List(String) = [1,2,3] x }");
}

#[test]
fn module_could_not_unify10() {
    assert_module_error!(
        "fn main() {
            let #(y, [..x]): #(x, List(x)) = #(\"one\", [1,2,3])
            x
        }"
    );
}

#[test]
fn module_could_not_unify11() {
    assert_module_error!(
        "
        pub type Box(inner) {
            Box(inner)
        }

        pub fn create_int_box(value: Int) {
            let x: Box(Float) = Box(value)
            x
        }"
    );
}

#[test]
fn module_could_not_unify12() {
    assert_module_error!(
        "
        pub type Person {
            Person(name: String, age: Int)
        }

        pub fn create_person(age: Float) {
            let x: Person = Person(name: \"Quinn\", age: age)
            x
        }"
    );
}

#[test]
fn module_arity_error() {
    assert_module_error!("external fn go(List(a, b)) -> a = \"\" \"\"");
}

#[test]
fn module_private_type_leak_1() {
    assert_module_error!(
        r#"external type PrivateType
pub external fn leak_type() -> PrivateType = "" """#
    );
}

#[test]
fn module_private_type_leak_2() {
    assert_module_error!(
        r#"external type PrivateType
external fn go() -> PrivateType = "" ""
pub fn leak_type() { go() }"#
    );
}

#[test]
fn module_private_type_leak_3() {
    assert_module_error!(
        r#"external type PrivateType
external fn go() -> PrivateType = "" ""
pub fn leak_type() { [go()] }"#
    );
}

#[test]
fn module_private_type_leak_4() {
    assert_module_error!(
        r#"external type PrivateType
pub external fn go(PrivateType) -> Int = "" """#
    );
}

#[test]
fn module_private_type_leak_5() {
    assert_module_error!(
        r#"external type PrivateType
pub type LeakType { Variant(PrivateType) }"#
    );
}

#[test]
fn unexpected_labelled_arg() {
    assert_module_error!(r#"fn id(x) { x } fn y() { id(x: 4) }"#);
}

#[test]
fn positional_argument_after_labelled() {
    assert_module_error!(
        r#"type X { X(a: Int, b: Int, c: Int) }
fn x() { X(b: 1, a: 1, 1) }"#
    );
}

#[test]
fn unknown_type() {
    assert_module_error!(r#"type Thing { Thing(unknown: x) }"#);
}

#[test]
fn unknown_type_in_alias() {
    // We cannot refer to unknown types in an alias
    assert_module_error!("type IntMap = IllMap(Int, Int)");
}

#[test]
fn unknown_type_in_alias2() {
    // We cannot refer to unknown types in an alias
    assert_module_error!("type IntMap = Map(Inf, Int)");
}

#[test]
fn unknown_type_var_in_alias2() {
    // We cannot use undeclared type vars in a type alias
    assert_module_error!("type X = List(a)");
}

#[test]
fn module_non_local_gaurd_var() {
    assert_module_error!(
        r#"fn one() { 1 }
fn main() { case 1 { _ if one -> 1 } }"#
    );
}

#[test]
fn unknown_record_field() {
    // An unknown field should report the possible fields' labels
    assert_module_error!(
        "
pub type Box(a) { Box(inner: a) }
pub fn main(box: Box(Int)) { box.unknown }
"
    );
}

#[test]
fn unknown_record_field_2() {
    // An unknown field should report the possible fields' labels
    assert_module_error!(
        "
pub type Box(a) { Box(inner: a) }
pub fn main(box: Box(Box(Int))) { box.inner.unknown }"
    );
}

#[test]
fn unnecessary_spread_operator() {
    assert_module_error!(
        "
type Triple {
  Triple(a: Int, b: Int, c: Int)
}

fn main() {
  let triple = Triple(1,2,3)
  let Triple(a, b, c, ..) = triple
  a
}"
    );
}

#[test]
fn duplicate_var_in_record_pattern() {
    // Duplicate var in record
    assert_module_error!(
        r#"type X { X(a: Int, b: Int, c: Int) }
fn x() {
  case X(1,2,3) { X(x, y, x) -> 1 }
}"#
    );
}

#[test]
fn guard_record_wrong_arity() {
    // Constructor in guard clause errors
    assert_module_error!(
        r#"type X { X(a: Int, b: Float) }
fn x() {
  case X(1, 2.0) { x if x == X(1) -> 1 }
}"#
    );
}

#[test]
fn subject_int_float_guard_tuple() {
    assert_module_error!(
        r#"type X { X(a: Int, b: Float) }
fn x() { case X(1, 2.0) { x if x == X(2.0, 1) -> 1 } }"#
    );
}

#[test]
fn type_variables_in_body() {
    // Type variables are shared between function annotations and let annotations within their body
    assert_module_error!(
        "
pub type Box(a) {
  Box(value: a)
}
pub fn go(box1: Box(a), box2: Box(b)) {
  let _: Box(a) = box2
  let _: Box(b) = box1
  5
}"
    );
}

#[test]
fn duplicate_function_names() {
    // We cannot declare two functions with the same name in a module
    assert_module_error!(
        "fn dupe() { 1 }
fn dupe() { 2 }"
    );
}

#[test]
fn duplicate_function_names_2() {
    // Different types to force a unify error if we don't detect the
    // duplicate during refactoring.
    assert_module_error!(
        "fn dupe() { 1 }
fn dupe() { 2.0 }"
    );
}

#[test]
fn duplicate_function_names_3() {
    assert_module_error!(
        "fn dupe() { 1 }
fn dupe(x) { x }"
    );
}

#[test]
fn duplicate_function_names_4() {
    assert_module_error!(
        "fn dupe() { 1 }
external fn dupe(x) -> x = \"\" \"\""
    );
}

#[test]
fn duplicate_function_names_5() {
    assert_module_error!(
        "external fn dupe(x) -> x = \"\" \"\"
fn dupe() { 1 }"
    );
}

#[test]
fn duplicate_constructors() {
    // We cannot declare two type constructors with the same name in a module
    assert_module_error!(
        "type Box { Box(x: Int) }
type Boxy { Box(Int) }"
    );
}

#[test]
fn duplicate_constructors2() {
    // We cannot declare two type constructors with the same name in a module
    assert_module_error!(
        "type Boxy { Box(Int) }
type Box { Box(x: Int) }"
    );
}

#[test]
fn duplicate_constructors3() {
    // We cannot declare two type constructors with the same name in a module
    assert_module_error!("type Boxy { Box(Int) Box(Float) }");
}

#[test]
fn duplicate_alias_names() {
    // We cannot reuse an alias name in the same module
    assert_module_error!("type X = Int type X = Int");
}

#[test]
fn duplicate_custom_type_names() {
    // We cannot declare two types with the same name in a module
    assert_module_error!("type DupType { A } type DupType { B }");
}

#[test]
fn duplicate_const_names() {
    // We cannot declare two const with the same name in a module
    assert_module_error!(
        "const duplicate = 1;
pub const duplicate = 1"
    );
}

#[test]
fn correct_pipe_arity_error_location() {
    // https://github.com/gleam-lang/gleam/issues/672
    assert_module_error!(
        "fn x(x, y) { x }
fn main() { 1 |> x() }"
    );
}

#[test]
fn const_annotation_wrong() {
    assert_module_error!("pub const group_id: Int = \"42\"");
}

#[test]
fn const_annotation_wrong_2() {
    assert_module_error!("pub const numbers: List(Int) = [1, 2, 2.3]");
}

#[test]
fn const_annotation_wrong_3() {
    assert_module_error!("pub const numbers: List(Int) = [1.1, 2.2, 3.3]");
}

#[test]
fn const_annotation_wrong_4() {
    assert_module_error!("pub const pair: #(Int, Float) = #(4.1, 1)");
}

#[test]
fn const_usage_wrong() {
    assert_module_error!(
        "const pair = #(1, 2.0)
fn main() { 1 == pair }"
    );
}

#[test]
fn const_heterogenus_list() {
    assert_module_error!("const pair = [1, 1.0]");
}

#[test]
fn custom_type_module_constants() {
    assert_module_error!(
        r#"type X { X }
const x = unknown.X"#
    );
}

#[test]
fn unknown_label() {
    assert_module_error!(
        r#"type X { X(a: Int, b: Float) }
fn x() {
  let x = X(a: 1, c: 2.0)
  x
}"#
    );
}

#[test]
fn wrong_type_var() {
    // A unification error should show the type var as named by user
    // See https://github.com/gleam-lang/gleam/issues/1256
    assert_module_error!(
        r#"fn foo(x: String) { x }
fn multi_result(x: some_name) {
  foo(x)
}"#
    );
}

#[test]
fn wrong_type_arg() {
    assert_module_error!(
        r#"
fn foo(x: List(Int)) { x }
fn main(y: List(something)) {
  foo(y)
}"#
    );
}

#[test]
fn wrong_type_ret() {
    // See https://github.com/gleam-lang/gleam/pull/1407#issuecomment-1001162876
    assert_module_error!(
        r#"pub fn main(x: something) -> Int {
  let y = x
  y
}"#
    );
}

#[test]
fn wrong_type_update() {
    // A variable of the wrong type given to a record update
    assert_module_error!(
        "
 pub type Person {
   Person(name: String, age: Int)
 };
 pub type Box(a) {
   Box(a)
 };
 pub fn update_person(person: Person, box: Box(a)) {
   Person(..box)
 }"
    );
}

#[test]
fn unknown_variable_update() {
    // An undefined variable given to a record update
    assert_module_error!(
        "
pub type Person {
  Person(name: String, age: Int)
};
pub fn update_person() {
  Person(..person)
}"
    );
}

#[test]
fn unknown_field_update() {
    // An unknown field given to a record update
    assert_module_error!(
        "
 pub type Person {
   Person(name: String)
 };
 pub fn update_person(person: Person) {
   Person(..person, one: 5)
 }"
    );
}

#[test]
fn unknown_field_update2() {
    // An unknown field given to a record update
    assert_module_error!(
        "
 pub type Person {
   Person(name: String, age: Int, size: Int)
 };
 pub fn update_person(person: Person) {
   Person(..person, size: 66, one: 5, age: 3)
 }"
    );
}

#[test]
fn unknown_constructor_update() {
    // An unknown record constructor being used in a record update
    assert_module_error!(
        "
pub type Person {
   Person(name: String, age: Int)
};
pub fn update_person(person: Person) {
   NotAPerson(..person)
}"
    );
}

#[test]
fn not_a_constructor_update() {
    // Something other than a record constructor being used in a record update
    assert_module_error!(
        "
pub type Person {
  Person(name: String, age: Int)
};
pub fn identity(a) { a }
pub fn update_person(person: Person) {
  identity(..person)
}"
    );
}

#[test]
fn expression_constructor_update() {
    // A record update with a constructor returned from an expression
    assert_module_error!(
        "
pub type Person {
  Person(name: String, age: Int)
};
pub fn update_person(person: Person) {
  let constructor = Person
  constructor(..person)
}"
    );
}

#[test]
fn generic_record_update1() {
    // A record update on polymorphic types with a field of the wrong type
    assert_module_error!(
        "
pub type Box(a) {
  Box(value: a, i: Int)
};
pub fn update_box(box: Box(Int), value: String) {
  Box(..box, value: value)
};"
    );
}

#[test]
fn generic_record_update2() {
    // A record update on polymorphic types with generic fields of the wrong type
    assert_module_error!(
        "
pub type Box(a) {
  Box(value: a, i: Int)
};
pub fn update_box(box: Box(a), value: b) {
  Box(..box, value: value)
};"
    );
}

#[test]
fn type_vars_must_be_declared() {
    // https://github.com/gleam-lang/gleam/issues/734
    assert_module_error!(
        r#"type A(a) { A };
type B = a"#
    );
}

#[test]
fn type_holes1() {
    // Type holes cannot be used when decaring types or external functions
    assert_module_error!(r#"type A { A(_) };"#);
}

#[test]
fn type_holes2() {
    // Type holes cannot be used when decaring types or external functions
    assert_module_error!(r#"external fn main() -> List(_) = "" """#);
}

#[test]
fn type_holes3() {
    // Type holes cannot be used when decaring types or external functions
    assert_module_error!(r#"external fn main(List(_)) -> Nil = "" """#);
}

#[test]
fn type_holes4() {
    // Type holes cannot be used when decaring types or external functions
    assert_module_error!(r#"type X = List(_)"#);
}

// https://github.com/gleam-lang/gleam/issues/1263
#[test]
fn missing_variable_in_alternative_pattern() {
    assert_error!("case [] { [x] | [] -> x _ -> 0 }");
}

#[test]
fn type_annotations() {
    assert_module_error!("fn inc(x: a) { x + 1 }");
}

// https://github.com/gleam-lang/gleam/issues/892
#[test]
fn case_clause_pipe_diagnostic() {
    assert_module_error!(
        r#"
pub fn change(x: String) -> String {
  ""
}

pub fn parse(input: BitString) -> String {
  case input {
    <<>> -> 1
    <<"(":utf8, b:binary>> ->
      parse(input)
      |> change
  }
}"#
    );
}

#[test]
fn let_exhaustiveness1() {
    assert_module_error!(
        r#"
pub fn main(b) {
    let True = b
    Nil
}
"#
    );
}

#[test]
fn let_exhaustiveness2() {
    assert_module_error!(
        r#"
pub fn main(r) {
    let Error(_) = r
    Nil
}
"#
    );
}

#[test]
fn let_exhaustiveness3() {
    assert_module_error!(
        r#"
pub type Media {
    Audio(BitString)
    Video(BitString)
    Text(String)
}
pub fn main(m) {
    let Video(_) = m
    Nil
}
"#
    );
}

#[test]
fn let_exhaustiveness4() {
    assert_module_error!(
        r#"
pub type Media {
    Audio(BitString)
    Video(BitString)
    Text(String)
}
pub fn main(m) {
    let Video(_) as v = m
    v
}
"#
    );
}

#[test]
fn case_exhaustiveness1() {
    assert_module_error!(
        r#"
pub fn main(b) {
    case b {
        True -> Nil
    }
}
"#
    );
}

#[test]
fn case_exhaustiveness2() {
    assert_module_error!(
        r#"
pub fn main(r) {
    case r {
        Error(_) -> Nil
    }
}
"#
    );
}

#[test]
fn case_exhaustiveness3() {
    assert_module_error!(
        r#"
pub type Media {
    Audio(BitString)
    Video(BitString)
    Text(String)
}
pub fn main(m) {
    case m {
        Audio(_) as a -> a
        Video(_) -> m
    }
}
"#
    );
}

#[test]
fn case_exhaustiveness4() {
    assert_module_error!(
        r#"
pub type Media {
    Audio(BitString)
    Video(BitString)
    Text(String)
}
pub fn main(m) {
    case m {
        Video(_) -> m
    }
}
"#
    );
}

#[test]
fn case_exhaustiveness5() {
    assert_module_error!(
        r#"
pub type Media {
    Audio(BitString)
    Video(BitString)
    Text(String)
}
pub fn main(m) {
    case m {
        Audio(_) | Text(_) -> m
    }
}
"#
    );
}

#[test]
fn case_exhaustiveness6() {
    assert_module_error!(
        r#"
pub fn main(b) {
    case b {
        b if b == True -> Nil
        b if b != True -> Nil
    }
}
"#
    );
}

#[test]
fn pipe_arity_error() {
    assert_module_error!(
        r#"
fn go(x, y) {
  x + y
}

fn main(x) {
  1
  |> go
}
"#
    );
}

#[test]
fn negate_string() {
    assert_error!(r#"!"Hello Gleam""#);
}
