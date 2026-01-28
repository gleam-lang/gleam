use crate::{
    assert_error, assert_internal_module_error, assert_js_module_error, assert_module_error,
    assert_module_syntax_error,
};

#[test]
fn bit_array_invalid_type() {
    assert_module_error!(
        "fn x() { \"test\" }

fn main() {
    let a = <<1:size(x())>>
    a
}"
    );
}

#[test]
fn bit_arrays2() {
    assert_error!("let <<x:utf8>> = <<1>>");
}

#[test]
fn bit_arrays3() {
    assert_error!("let <<x:utf16>> = <<1>>");
}

#[test]
fn bit_arrays4() {
    assert_error!("let <<x:utf32>> = <<1>>");
}

#[test]
fn bit_array_float() {
    assert_error!("case <<1>> { <<a:float>> if a > 1 -> 1 _ -> 2 }");
}

#[test]
fn bit_array_binary() {
    assert_error!("case <<1>> { <<a:bytes>> if a > 1 -> 1 _ -> 2 }");
}

#[test]
fn bit_array_guard() {
    assert_error!("case <<1>> { <<a:utf16_codepoint>> if a == \"test\" -> 1 _ -> 2 }");
}

#[test]
fn bit_array_segment_nosize() {
    assert_error!("case <<1>> { <<_:bytes, _:bytes>> -> 1 }");
}

#[test]
fn bit_array_segment_nosize2() {
    assert_error!("case <<1>> { <<_:bits, _:bytes>> -> 1 }");
}

#[test]
fn bit_array_segment_nosize3() {
    assert_error!("case <<1>> { <<_:bytes, _:bits>> -> 1 }");
}

#[test]
fn bit_array_segment_conflicting_options_int() {
    assert_error!("let x = <<1:int-bytes>> x");
}

#[test]
fn bit_array_segment_conflicting_options_bit_array() {
    assert_error!("case <<1>> { <<1:bits-bytes>> -> 1 }");
}

#[test]
fn bit_array_segment_conflicting_signedness1() {
    assert_error!("let x = <<1:signed-unsigned>> x");
}

#[test]
fn bit_array_segment_conflicting_signedness2() {
    assert_error!("case <<1>> { <<1:unsigned-signed>> -> 1 }");
}

#[test]
fn bit_array_segment_conflicting_endianness1() {
    assert_error!("let x = <<1:big-little>> x");
}

#[test]
fn bit_array_segment_conflicting_endianness2() {
    assert_error!("case <<1>> { <<1:native-big>> -> 1 }");
}

#[test]
fn bit_array_segment_size() {
    assert_error!("let x = <<1:8-size(5)>> x");
}

#[test]
fn bit_array_segment_size2() {
    assert_error!("case <<1>> { <<1:size(2)-size(8)>> -> 1 }");
}

#[test]
fn bit_array_segment_unit_unit() {
    assert_error!("let x = <<1:unit(2)-unit(5)>> x");
}

#[test]
fn bit_array_segment_type_does_not_allow_unit_codepoint_utf8() {
    assert_error!("let x = <<1:utf8_codepoint-unit(5)>> x");
}

#[test]
fn bit_array_segment_type_does_not_allow_unit_codepoint_utf16() {
    assert_error!("let x = <<1:utf16_codepoint-unit(5)>> x");
}

#[test]
fn bit_array_segment_type_does_not_allow_unit_codepoint_utf32() {
    assert_error!("case <<1>> { <<1:utf32_codepoint-unit(2)>> -> 1 }");
}

#[test]
fn bit_array_segment_type_does_not_allow_unit_codepoint_utf8_2() {
    assert_error!("let x = <<1:utf8_codepoint-size(5)>> x");
}

#[test]
fn bit_array_segment_type_does_not_allow_unit_codepoint_utf16_2() {
    assert_error!("let x = <<1:utf16_codepoint-size(5)>> x");
}

#[test]
fn bit_array_segment_type_does_not_allow_unit_codepoint_utf32_2() {
    assert_error!("case <<1>> { <<1:utf32_codepoint-size(5)>> -> 1 }");
}

#[test]
fn bit_array_segment_type_does_not_allow_unit_utf8_2() {
    assert_error!("let x = <<1:utf8-unit(5)>> x");
}

#[test]
fn bit_array_segment_type_does_not_allow_unit_utf16() {
    assert_error!("let x = <<1:utf16-unit(5)>> x");
}

#[test]
fn bit_array_segment_type_does_not_allow_unit_utf32() {
    assert_error!("case <<1>> { <<1:utf32-unit(2)>> -> 1 }");
}

#[test]
fn bit_array_segment_type_does_not_allow_size_utf8() {
    assert_error!("let x = <<1:utf8-size(5)>> x");
}

#[test]
fn bit_array_segment_type_does_not_allow_size_utf16() {
    assert_error!("let x = <<1:utf16-size(5)>> x");
}

#[test]
fn bit_array_segment_type_does_not_allow_size_utf32() {
    assert_error!("case <<1>> { <<1:utf32-size(5)>> -> 1 }");
}

#[test]
fn bit_array_segment_type_does_not_allow_variable_string() {
    assert_error!("case <<>> { <<a:utf8>> -> 1 _ -> 2 }");
}

#[test]
fn bit_array_segment_type_does_not_allow_aliased_variable_string() {
    assert_error!("case <<>> { <<_ as a:utf8>> -> 1 _ -> 2 }");
}

#[test]
fn bit_array_segment_unit_no_size() {
    assert_error!("let x = <<1:unit(5)>> x");
}

#[test]
fn bit_array_size_not_int() {
    assert_error!("let x = <<1:size(\"1\")>> x");
}

#[test]
fn bit_array_size_not_int_variable() {
    assert_error!("let a = 2.0 case <<1>> { <<1:size(a)>> -> a }");
}

#[test]
fn bit_array_float_size() {
    // float given invalid size
    assert_error!("let x = <<1:8-float>> x");
}

#[test]
fn bit_array_bits_option_in_value() {
    assert_error!("let x = <<<<1:1>>:bytes>> x");
}

#[test]
fn bit_array_utf8_and_size() {
    assert_error!(r#"let x = <<"test":size(1)>> x"#);
}

#[test]
fn bit_array_utf8_and_unit() {
    assert_error!(r#"let x = <<"test":unit(5)>> x"#);
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
fn unknown_variable_type() {
    assert_error!("Int");
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
fn functions_called_outside_module() {
    assert_module_syntax_error!("const first = list.at([1], 0)");
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
fn pipe_value_type_mismatch_error() {
    assert_module_error!(
        "pub fn main() -> String {
            eat_veggie
            |> Orange
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
    assert_error!("case [1] { x if x == [1, 2.0] -> 1 _ -> 2 }");
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
fn wrong_number_of_subjects_alternative_patterns() {
    assert_error!("case 1 { _, _ | _ | _, _, _ -> 1 }");
}

#[test]
fn recursive_var() {
    assert_error!("let id = fn(x) { x(x) } 1");
}

#[test]
fn true_fn() {
    assert_error!("let True(x) = 1");
}

#[test]
fn ok_2_args() {
    assert_error!("let Ok(1, x) = 1");
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
    assert_error!("let #(a, b) = 1");
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
fn int_operator_on_floats_in_case_guard() {
    assert_error!("case 3.0 { x if x > 2.0 -> \"a\" _ -> \"b\" }");
}

#[test]
fn float_operator_on_ints_in_case_guard() {
    assert_error!("case 3 { x if x +. 2 == 5.0 -> \"a\" _ -> \"b\" }");
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
    assert_error!("case [3.33], 1 { x, x -> 1 }");
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
"
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
}

#[test]
fn accessor_multiple_variants_multiple_positions2() {
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
fn record_access_on_inferred_variant_when_field_is_in_other_variants() {
    assert_module_error!(
        "
pub type Wibble {
  Wibble(wibble: Int)
  Wobble(wobble: Int)
}

pub fn main() {
  let always_wibble = Wibble(10)
  always_wibble.wobble
}
"
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
fn wobble() -> Int {
    5
}

fn run(one: fn() -> String) {
    one()
}

fn demo() {
    run(wobble)
}"
    );
}

#[test]
fn module_could_not_unify5() {
    assert_module_error!(
        "
fn wobble(x: Int) -> Int {
    x * 5
}

fn run(one: fn(String) -> Int) {
    one(\"one.\")
}

fn demo() {
    run(wobble)
}"
    );
}

#[test]
fn module_could_not_unify6() {
    assert_module_error!("fn main() { let x: String = 5 x }");
}

#[test]
fn module_could_not_unify7() {
    assert_module_error!("fn main() { let assert 5 = \"\" }");
}

#[test]
fn module_could_not_unify8() {
    assert_module_error!("fn main() { let x: #(x, x) = #(5, 5.0) x }");
}

#[test]
fn module_could_not_unify9() {
    assert_module_error!("fn main() { let assert [1, 2, ..x]: List(String) = [1,2,3] x }");
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
    assert_module_error!("fn go(x: List(a, b)) -> Int { 1 }");
}

#[test]
fn module_private_type_leak_1() {
    assert_module_error!(
        r#"type PrivateType

@external(erlang, "a", "b")
pub fn leak_type() -> PrivateType
"#
    );
}

#[test]
fn module_private_type_leak_2() {
    assert_module_error!(
        r#"type PrivateType

@external(erlang, "a", "b")
fn go() -> PrivateType

pub fn leak_type() { go() }"#
    );
}

#[test]
fn module_private_type_leak_3() {
    assert_module_error!(
        r#"type PrivateType
@external(erlang, "a", "b")
fn go() -> PrivateType
pub fn leak_type() { [go()] }"#
    );
}

#[test]
fn module_private_type_leak_4() {
    assert_module_error!(
        r#"type PrivateType
@external(erlang, "a", "b")
pub fn go(x: PrivateType) -> Int"#
    );
}

#[test]
fn module_private_type_leak_5() {
    assert_module_error!(
        r#"type PrivateType
pub type LeakType { Variant(PrivateType) }"#
    );
}

// https://github.com/gleam-lang/gleam/issues/3387
// Private types should not leak even in internal modules
#[test]
fn module_private_type_leak_6() {
    assert_internal_module_error!(
        r#"type PrivateType
pub type LeakType { Variant(PrivateType) }"#
    );
}

#[test]
fn unexpected_labelled_arg() {
    assert_module_error!(r#"fn id(x) { x } fn y() { id(x: 4) }"#);
}

#[test]
fn unexpected_labelled_arg_record_constructor() {
    assert_module_error!(r#"type X { X(Int) } fn y() { X(a: 0) }"#);
}

#[test]
fn unexpected_arg_with_label_shorthand() {
    assert_module_error!(
        r#"
    fn id(x) { x }
    fn y() {
        let x = 4
        id(x:)
    }
"#
    );
}

#[test]
fn positional_argument_after_labelled() {
    assert_module_error!(
        r#"type X { X(a: Int, b: Int, c: Int) }
fn x() { X(b: 1, a: 1, 1) }"#
    );
}

#[test]
fn positional_argument_after_one_using_label_shorthand() {
    assert_module_error!(
        r#"type X { X(a: Int, b: Int, c: Int) }
fn x() {
  let b = 1
  let a = 1
  X(b:, a:, 1)
}"#
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
fn duplicate_label_shorthands_in_record_pattern() {
    // Duplicate var in record
    assert_module_error!(
        r#"type X { X(a: Int, b: Int, c: Int) }
fn x() {
  case X(1,2,3) { X(a:, b:, c: a) -> 1 }
}"#
    );
}

#[test]
fn guard_record_wrong_arity() {
    // Constructor in guard clause errors
    assert_module_error!(
        r#"type X { X(a: Int, b: Float) }
fn x() {
  case X(1, 2.0) { x if x == X(1) -> 1 _ -> 2 }
}"#
    );
}

#[test]
fn subject_int_float_guard_tuple() {
    assert_module_error!(
        r#"type X { X(a: Int, b: Float) }
fn x() { case X(1, 2.0) { x if x == X(2.0, 1) -> 1 _ -> 2 } }"#
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
        r#"fn dupe() { 1 }
@external(erlang, "a", "b")
fn dupe(x) -> x
"#
    );
}

#[test]
fn duplicate_function_names_5() {
    assert_module_error!(
        r#"
@external(erlang, "a", "b")
fn dupe(x) -> x
fn dupe() { 1 }
"#
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
        "const duplicate = 1
pub const duplicate = 1"
    );
}

#[test]
fn duplicate_const_and_function_names_const_fn() {
    // We cannot declare const and functions with the same name in a module
    // https://github.com/gleam-lang/gleam/issues/2069
    assert_module_error!(
        "const duplicate = 1
fn duplicate() { 2 }"
    );
}

#[test]
fn duplicate_const_const() {
    assert_module_error!(
        "const wibble = 1
const wibble = 2"
    );
}

#[test]
fn duplicate_fn_fn() {
    assert_module_error!(
        "fn wibble() { 1 }
fn wibble() { 2 }"
    );
}

#[test]
fn duplicate_extfn_extfn() {
    assert_module_error!(
        r#"
@external(erlang, "module1", "function1")
fn wibble() -> Float
@external(erlang, "module2", "function2")
fn wibble() -> Float
"#
    );
}

#[test]
fn duplicate_extfn_fn() {
    assert_module_error!(
        "
@external(erlang, \"module1\", \"function1\")
fn wibble() -> Float

fn wibble() { 2 }"
    );
}

#[test]
fn duplicate_fn_extfn() {
    assert_module_error!(
        "fn wibble() { 1 }

@external(erlang, \"module2\", \"function2\")
fn wibble() -> Float
"
    );
}

#[test]
fn duplicate_const_extfn() {
    assert_module_error!(
        "const wibble = 1

@external(erlang, \"module2\", \"function2\")
fn wibble() -> Float
"
    );
}

#[test]
fn duplicate_extfn_const() {
    assert_module_error!(
        "
@external(erlang, \"module1\", \"function1\")
fn wibble() -> Float

const wibble = 2"
    );
}

#[test]
fn duplicate_const_fn() {
    assert_module_error!(
        "const wibble = 1
fn wibble() { 2 }"
    );
}

#[test]
fn duplicate_fn_const() {
    assert_module_error!(
        "fn wibble() { 1 }
const wibble = 2"
    );
}

#[test]
fn invalid_const_name() {
    assert_module_error!("const myInvalid_Constant = 42");
}

#[test]
fn invalid_parameter_name() {
    assert_module_error!("fn add(numA: Int, num_b: Int) { numA + num_b }");
}

#[test]
fn invalid_parameter_name2() {
    assert_module_error!("fn pass(label paramName: Bool) { paramName }");
}

#[test]
fn invalid_parameter_name3() {
    assert_error!("let add = fn(numA: Int, num_b: Int) { numA + num_b }");
}

#[test]
fn invalid_parameter_discard_name() {
    assert_module_error!("fn ignore(_ignoreMe: Bool) { 98 }");
}

#[test]
fn invalid_parameter_discard_name2() {
    assert_module_error!("fn ignore(labelled_discard _ignoreMe: Bool) { 98 }");
}

#[test]
fn invalid_parameter_discard_name3() {
    assert_error!("let ignore = fn(_ignoreMe: Bool) { 98 }");
}

#[test]
fn invalid_parameter_label() {
    assert_module_error!("fn func(thisIsALabel param: Int) { param }");
}

#[test]
fn invalid_parameter_label2() {
    assert_module_error!("fn ignore(thisIsALabel _ignore: Int) { 25 }");
}

#[test]
fn invalid_constructor_name() {
    assert_module_error!("type MyType { Int_Value(Int) }");
}

#[test]
fn invalid_constructor_arg_name() {
    assert_module_error!("type IntWrapper { IntWrapper(innerInt: Int) }");
}

#[test]
fn invalid_custom_type_name() {
    assert_module_error!("type Boxed_value { Box(Int) }");
}

#[test]
fn invalid_type_alias_name() {
    assert_module_error!("type Fancy_Bool = Bool");
}

#[test]
fn invalid_function_name() {
    assert_module_error!("fn doStuff() {}");
}

#[test]
fn invalid_variable_name() {
    assert_error!("let theAnswer = 42");
}

#[test]
fn invalid_variable_discard_name() {
    assert_error!("let _boringNumber = 72");
}

#[test]
fn invalid_use_name() {
    assert_module_error!(
        "fn use_test(f) { f(Nil) }
pub fn main() { use useVar <- use_test() }"
    );
}

#[test]
fn invalid_use_discard_name() {
    assert_module_error!(
        "fn use_test(f) { f(Nil) }
pub fn main() { use _discardVar <- use_test() }"
    );
}

#[test]
fn invalid_pattern_assignment_name() {
    assert_error!("let assert 42 as theAnswer = 42");
}

#[test]
fn invalid_list_pattern_name() {
    assert_error!("let assert [theElement] = [9.4]");
}

#[test]
fn invalid_list_pattern_discard_name() {
    assert_error!("let assert [_elemOne] = [False]");
}

#[test]
fn invalid_constructor_pattern_name() {
    assert_module_error!(
        "pub type Box { Box(Int) } pub fn main() { let Box(innerValue) = Box(203) }"
    );
}

#[test]
fn invalid_constructor_pattern_discard_name() {
    assert_module_error!(
        "pub type Box { Box(Int) } pub fn main() { let Box(_ignoredInner) = Box(203)}"
    );
}

#[test]
fn invalid_tuple_pattern_name() {
    assert_error!("let #(a, secondValue) = #(1, 2)");
}

#[test]
fn invalid_tuple_pattern_discard_name() {
    assert_error!("let #(a, _secondValue) = #(1, 2)");
}

#[test]
fn invalid_bit_array_pattern_name() {
    assert_error!("let assert <<bitValue>> = <<73>>");
}

#[test]
fn invalid_bit_array_pattern_discard_name() {
    assert_error!("let assert <<_iDontCare>> = <<97>>");
}

#[test]
fn invalid_string_prefix_pattern_name() {
    assert_error!(r#"let assert "prefix" <> coolSuffix = "prefix-suffix""#);
}

#[test]
fn invalid_string_prefix_pattern_discard_name() {
    assert_error!(r#"let assert "prefix" <> _boringSuffix = "prefix-suffix""#);
}

#[test]
fn invalid_string_prefix_pattern_alias() {
    assert_error!(r#"let assert "prefix" as thePrefix <> _suffix = "prefix-suffix""#);
}

#[test]
fn invalid_case_variable_name() {
    assert_error!("case 21 { twentyOne -> {Nil} }");
}

#[test]
fn invalid_case_variable_discard_name() {
    assert_error!("case 21 { _twentyOne -> {Nil} }");
}

#[test]
fn invalid_type_parameter_name() {
    assert_module_error!("type Wrapper(innerType) {}");
}

#[test]
fn invalid_type_alias_parameter_name() {
    assert_module_error!("type GleamOption(okType) = Result(okType, Nil)");
}

#[test]
fn invalid_function_type_parameter_name() {
    assert_module_error!("fn identity(value: someType) { value }");
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
fn const_multiple_errors_mismatched_types() {
    assert_module_error!(
        "const mismatched_types: String = 7
const invalid_annotation: MyInvalidType = \"str\""
    );
}

#[test]
fn const_multiple_errors_invalid_annotation() {
    assert_module_error!(
        "const invalid_annotation: MyInvalidType = \"str\"
const invalid_value: String = MyInvalidValue"
    );
}

#[test]
fn const_multiple_errors_invalid_value() {
    assert_module_error!(
        "const invalid_value: String = MyInvalidValue
const invalid_unannotated_value = [1, 2.0]"
    );
}

#[test]
fn const_multiple_errors_invalid_unannotated_value() {
    assert_module_error!(
        "const invalid_unannotated_value = [1, 2.0]
const invalid_everything: MyInvalidType = MyInvalidValue"
    );
}

#[test]
fn const_multiple_errors_invalid_annotation_and_value() {
    assert_module_error!(
        "const invalid_everything: MyInvalidType = MyInvalidValue
const mismatched_types: String = 7"
    );
}

#[test]
fn const_multiple_errors_are_local_with_annotation() {
    assert_module_error!(
        "const num: String = 7
const tpl: String = #(Ok(1), MyInvalidType, 3)
const assignment1: String = num
const assignment2: String = tpl"
    );
}

#[test]
fn const_multiple_errors_are_local_with_inferred_value() {
    assert_module_error!(
        "const str: MyInvalidType = \"str\"
const assignment: String = str"
    );
}

#[test]
fn const_multiple_errors_are_local_with_unbound_value() {
    assert_module_error!(
        "const lst = [1, 2.0]
const unbound: MyInvalidType = MyInvalidType
const assignment1: String = lst
const assignment2: String = unbound"
    );
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
fn unknown_label_shorthand() {
    assert_module_error!(
        r#"type X { X(a: Int, b: Float) }
fn x() {
  let c = 2.0
  let x = X(a: 1, c:)
  x
}"#
    );
}

#[test]
fn wrong_type_var() {
    // A unification error should show the type var as named by user
    // See https://github.com/gleam-lang/gleam/issues/1256
    assert_module_error!(
        r#"fn wibble(x: String) { x }
fn multi_result(x: some_name) {
  wibble(x)
}"#
    );
}

#[test]
fn wrong_type_arg() {
    assert_module_error!(
        r#"
fn wibble(x: List(Int)) { x }
fn main(y: List(something)) {
  wibble(y)
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
 }
 pub type Box(a) {
   Box(a)
 }
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
}
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
 }
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
 }
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
}
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
}
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
}
pub fn update_person(person: Person) {
  let constructor = Person
  constructor(..person)
}"
    );
}

#[test]
fn type_vars_must_be_declared() {
    // https://github.com/gleam-lang/gleam/issues/734
    assert_module_error!(
        r#"type A(a) { A }
type B = a"#
    );
}

#[test]
fn type_holes1() {
    // Type holes cannot be used when decaring types or external functions
    assert_module_error!(r#"type A { A(_) }"#);
}

#[test]
fn type_holes2() {
    // Type holes cannot be used when decaring types or external functions
    assert_module_error!(
        r#"
@external(erlang, "a", "b")
fn main() -> List(_)
"#
    );
}

#[test]
fn type_holes3() {
    // Type holes cannot be used when decaring types or external functions
    assert_module_error!(
        r#"
@external(erlang, "a", "b")
fn main(x: List(_)) -> Nil
"#
    );
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

pub fn parse(input: BitArray) -> String {
  case input {
    <<>> -> 1
    <<"(":utf8, b:bytes>> ->
      parse(input)
      |> change
    _ -> 3
  }
}"#
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

#[test]
fn ambiguous_type_error() {
    assert_module_error!(
        ("wibble", "pub type Thing { Thing }"),
        "import wibble pub type Thing { Thing }
        pub fn main() {
            [Thing] == [wibble.Thing]
        }",
    );
}

#[test]
fn ambiguous_import_error_no_unqualified() {
    assert_module_error!(
        ("wibble/sub", "pub fn wobble() { 1 }"),
        ("wibble2/sub", "pub fn wobble() { 1 }"),
        "
        import wibble/sub
        import wibble2/sub
        pub fn main() {
            sub.wobble()
        }
        ",
    );
}

#[test]
fn ambiguous_import_error_with_unqualified() {
    assert_module_error!(
        ("wibble/sub", "pub fn wobble() { 1 }"),
        ("wibble2/sub", "pub fn wobble() { 1 }"),
        "
        import wibble/sub
        import wibble2/sub.{wobble}
        pub fn main() {
            sub.wobble()
        }
        ",
    );
}

#[test]
fn same_imports_multiple_times() {
    assert_module_error!(
        (
            "gleam/wibble",
            "
            pub fn wobble() { 1 }
            pub fn zoo() { 1 }
            "
        ),
        "
        import gleam/wibble.{wobble}
        import gleam/wibble.{zoo}
        pub fn go() { wobble() + zoo() }
        "
    );
}

#[test]
fn same_imports_multiple_times_1() {
    assert_module_error!(
        (
            "one",
            "
            pub fn fn1() { 1 }
            "
        ),
        (
            "two",
            "
            pub fn fn2() { 1 }
            "
        ),
        "
        import one
        import two as one
        "
    );
}

#[test]
fn same_imports_multiple_times_2() {
    assert_module_error!(
        (
            "one",
            "
            pub fn fn1() { 1 }
            "
        ),
        (
            "two",
            "
            pub fn fn2() { 1 }
            "
        ),
        "
        import one as two
        import two
        "
    );
}

#[test]
fn same_imports_multiple_times_3() {
    assert_module_error!(
        (
            "one",
            "
            pub fn fn1() { 1 }
            "
        ),
        (
            "two",
            "
            pub fn fn2() { 1 }
            "
        ),
        "
        import one as x
        import two as x
        "
    );
}

#[test]
fn same_imports_multiple_times_4() {
    assert_module_error!(
        (
            "one",
            "
            pub fn fn1() { 1 }
            "
        ),
        (
            "two",
            "
            pub fn fn2() { 1 }
            "
        ),
        "
        import one.{fn1}
        import two.{fn2} as one
        "
    );
}

#[test]
fn same_imports_multiple_times_5() {
    assert_module_error!(
        (
            "one",
            "
            pub fn fn1() { 1 }
            "
        ),
        (
            "two",
            "
            pub fn fn2() { 1 }
            "
        ),
        "
        import one.{fn1} as two
        import two.{fn2}
        "
    );
}

#[test]
fn same_imports_multiple_times_6() {
    assert_module_error!(
        (
            "one",
            "
            pub fn fn1() { 1 }
            "
        ),
        (
            "two",
            "
            pub fn fn2() { 1 }
            "
        ),
        "
        import one.{fn1} as x
        import two.{fn2} as x
        "
    );
}

#[test]
fn same_imports_multiple_times_7() {
    assert_module_error!(
        (
            "one",
            "
            pub fn fn1() { 1 }
            "
        ),
        (
            "two",
            "
            pub fn fn2() { 1 }
            "
        ),
        "
        import one.{
          fn1
        } as x
        import two.{
          fn2
        } as x
        "
    );
}

// https://github.com/gleam-lang/gleam/issues/1705
#[test]
fn update_multi_variant_record() {
    assert_module_error!(
        "
pub type Point {
  Point2(a: Int, b: Int)
  Point3(a: Int, b: Int, c: Int)
}

pub fn main() {
  Point3(..Point2(a: 1, b: 2))
}"
    );
}

#[test]
fn hint_for_method_call() {
    assert_module_error!(
        "
pub type User {
  User(id: Int, name: String)
}

pub fn main(user: User) {
  user.login()
}
"
    );
}

#[test]
fn no_hint_for_non_method_call() {
    assert_module_error!(
        "
pub type User {
  User(id: Int, name: String)
}

fn login(user: User) {
  user
}

pub fn main(user: User) {
  login(user.wibble)
}
"
    );
}

#[test]
fn unknown_imported_module_type() {
    assert_module_error!(
        ("one/two", ""),
        "
import one/two

pub fn main(_x: two.Thing) {
  Nil
}
"
    );
}

#[test]
fn value_imported_as_type() {
    assert_module_error!(
        (
            "gleam/wibble",
            "pub type Wibble {
               Wobble
             }"
        ),
        "import gleam/wibble.{type Wobble}"
    );
}

#[test]
fn type_imported_as_value() {
    assert_module_error!(
        (
            "gleam/wibble",
            "pub type Wibble {
               Wobble
             }"
        ),
        "import gleam/wibble.{Wibble}"
    );
}

#[test]
fn duplicate_module_function_arguments() {
    assert_module_error!(
        "
pub fn main(x, x) {
  Nil
}
"
    );
}

#[test]
fn duplicate_anon_function_arguments() {
    assert_error!(
        "
fn(x, x) {
  Nil
}
"
    );
}

#[test]
fn negate_boolean_as_integer() {
    assert_error!(
        "
fn() {
  let a = True
  let b = -a
}
"
    );
}

#[test]
fn negate_float_as_integer() {
    assert_error!(
        "
fn() {
  let a = 3.0
  let b = -a
}
"
    );
}

// https://github.com/gleam-lang/gleam/issues/2371
#[test]
fn list() {
    assert_error!("[1, 2.0]");
}

#[test]
fn mismatched_list_tail() {
    assert_error!("[\"wibble\", ..[1, 2]]");
}

#[test]
fn leak_multiple_private_types() {
    assert_module_error!(
        "
        type Private {
            Private
        }

        pub fn ret_private() -> Private {
            Private
        }

        pub fn ret_private2() -> Private {
            Private
        }

        pub fn main() {
            ret_private()
        }
        "
    );
}

#[test]
fn const_string_concat_invalid_type() {
    assert_module_error!(
        "
const some_int = 5
const invalid_concat = some_int <> \"with_string\"
"
    );
}

#[test]
fn invalid_pattern_label_shorthand() {
    assert_module_error!(
        "
pub type Wibble { Wibble(arg: Int) }
pub fn main() {
  let Wibble(not_a_label:) = Wibble(1)
}
"
    );
}

#[test]
fn no_crash_on_duplicate_definition() {
    // This previous caused the compiler to crash
    assert_module_error!(
        "
pub type Wibble {
  Wobble
  Wobble
}

pub fn main() {
  let wibble = Wobble
  case wibble {
    Wobble -> Nil
  }
}
"
    );
}

#[test]
fn no_crash_on_duplicate_definition2() {
    // This also caused a compiler crash, separate to the above test
    assert_module_error!(
        "
pub type Wibble {
  Wibble
  Wobble
  Wobble
  Wubble
}

pub fn main() {
  let wibble = Wobble
  case wibble {
    Wibble -> Nil
    Wobble -> Nil
    Wubble -> Nil
  }
}
"
    );
}

#[test]
fn unknown_module_suggest_import() {
    assert_module_error!(
        ("utils", "pub fn helpful() {}"),
        "
pub fn main() {
  utils.helpful()
}
",
    );
}

#[test]
fn unknown_module_suggest_typo_for_imported_module() {
    assert_module_error!(
        ("wibble", "pub fn wobble() {}"),
        "
import wibble
pub fn main() {
  wible.wobble()
}
",
    );
}

#[test]
fn unknown_module_suggest_typo_for_unimported_module() {
    assert_module_error!(
        ("wibble/wobble", "pub fn wubble() {}"),
        "
pub fn main() {
  woble.wubble()
}
",
    );
}

#[test]
fn qualified_type_mismatched_type_error() {
    assert_module_error!(
        ("wibble", "pub type Wobble"),
        "
import wibble
const my_wobble: wibble.Wobble = Nil
"
    );
}

#[test]
fn qualified_type_similar_type_name() {
    assert_module_error!(
        ("wibble", "pub type Int"),
        "
import wibble
const value: wibble.Int = 20
"
    );
}

#[test]
fn qualified_type_not_a_function() {
    assert_module_error!(
        ("wibble", "pub type Function { Function(fn() -> Nil) }"),
        "
import wibble.{type Function as FuncWrapper}
pub fn main(f: FuncWrapper) {
  f()
}
"
    );
}

#[test]
fn qualified_type_unknown_field() {
    assert_module_error!(
        "
import gleam
type Int {
  Int(bit_size: gleam.Int, bits: BitArray)
}

pub fn main(not_a_record: gleam.Int) {
  not_a_record.bits
}
"
    );
}

#[test]
fn qualified_type_invalid_operands() {
    assert_module_error!(
        ("maths", "pub type Vector { Vector(x: Float, y: Float) }"),
        "
import maths as math
pub fn add_two_vectors(a: math.Vector, b: math.Vector) {
  a + b
}
"
    );
}

#[test]
fn qualified_type_invalid_pipe_argument() {
    assert_module_error!(
        (
            "mod",
            "pub type Wibble pub fn takes_wibble(value: Wibble) { value }"
        ),
        "
import mod
pub fn main() {
  Nil |> mod.takes_wibble
}
"
    );
}

#[test]
fn qualified_type_unification_error() {
    assert_module_error!(
        "
import gleam

type Bool {
  True
  False
}

const list_of_bools = [True, False, gleam.False]
"
    );
}

#[test]
fn qualified_type_not_a_tuple() {
    assert_module_error!(
        ("mod", "pub type Pair(a, b) { Pair(a, b) }"),
        "
import mod.{type Pair as Duo}
pub fn first(pair: Duo(a, b)) {
  pair.0
}
"
    );
}

#[test]
fn qualified_type_not_fn_in_use() {
    assert_module_error!(
        ("some_mod", "pub type Function(param1, param2, return)"),
        "
import some_mod as sm
pub fn main(func: sm.Function(Int, String, Float)) {
  use <- func()
}
"
    );
}

#[test]
fn qualified_type_use_fn_without_callback() {
    assert_module_error!(
        (
            "some_mod",
            "pub type NotACallback pub fn do_a_thing(a: Int, _b: NotACallback) { a }"
        ),
        "
import some_mod
pub fn main() {
  use value <- some_mod.do_a_thing(10)
}
"
    );
}

#[test]
fn suggest_unwrapping_a_result_when_types_match() {
    assert_module_error!(
        "
pub fn main() {
  let value = Ok(1)
  add_1(value)
}

fn add_1(to x) { x + 1 }
"
    );
}

#[test]
fn unknown_field_that_appears_in_an_imported_variant_has_note() {
    assert_module_error!(
        (
            "some_mod",
            "pub type Wibble {
              Wibble(field: Int)
              Wobble(not_field: String, field: Int)
            }"
        ),
        "
import some_mod
pub fn main(wibble: some_mod.Wibble) {
  wibble.field
}
"
    );
}

#[test]
fn unknown_field_that_appears_in_a_variant_has_note() {
    assert_module_error!(
        "
pub type Wibble {
  Wibble(field: Int)
  Wobble(not_field: String, field: Int)
}

pub fn main(wibble: Wibble) {
  wibble.field
}
"
    );
}

#[test]
fn unknown_field_that_does_not_appear_in_variant_has_no_note() {
    assert_module_error!(
        "
pub type Wibble {
  Wibble(field: Int)
  Wobble(not_field: String, field: Int)
}

pub fn main(wibble: Wibble) {
  wibble.wibble
}
"
    );
}

#[test]
fn no_note_about_reliable_access_if_the_accessed_type_has_a_single_variant() {
    assert_module_error!(
        "
pub type User {
  User(name: String)
}

pub fn main() {
  User(\"Jak\").nam
}
"
    );
}

#[test]
fn no_crash_on_duplicate_record_fields() {
    // https://github.com/gleam-lang/gleam/issues/3713
    assert_module_error!(
        "
pub type X {
  A
  B(e0: Int, e0: Int)
}

fn compiler_crash(x: X) {
  case x {
    A -> todo
    _ -> todo
  }
}
  "
    );
}

#[test]
fn record_update_unknown_variant() {
    assert_module_error!(
        r#"
pub type Wibble {
  Wibble(wibble: Int, wubble: Bool)
  Wobble(wobble: Int, wubble: Bool)
}

pub fn wibble(value: Wibble) {
  Wibble(..value, wubble: True)
}
"#
    );
}

#[test]
fn record_update_wrong_variant() {
    assert_module_error!(
        r#"
pub type MyRecord {
  A(common: Int, other: String)
  B(common: Int, different: Float)
}

pub fn b_to_a(value: MyRecord) {
  case value {
    A(..) -> value
    B(..) as b -> A(..b, other: "Hi")
  }
}
"#
    );
}

#[test]
fn record_update_wrong_variant_imported_type() {
    assert_module_error!(
        (
            "wibble",
            "
pub type Wibble {
  Wibble(wibble: Int, wobble: Int)
  Wobble(wobble: Int, wubble: Int)
}"
        ),
        "
import wibble

pub fn main(wibble: wibble.Wibble) {
  case wibble {
    wibble.Wibble(..) as w -> wibble.Wobble(..w, wubble: 10)
    _ -> panic
  }
}
"
    );
}

#[test]
fn inferred_variant_record_update_change_type_parameter_different_branches() {
    assert_module_error!(
        r#"
pub type Box(a) {
  Locked(password: String, value: a)
  Unlocked(password: String, value: a)
}

pub fn main() {
  let box = Locked("ungu€$$4bLe", 11)
  case box {
    Locked(..) as box -> Locked(..box, value: True)
    Unlocked(..) as box -> Unlocked(..box, password: "pwd")
  }
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/3783
#[test]
fn duplicate_fields_in_record_update_reports_error() {
    assert_module_error!(
        "
pub type Wibble { Wibble(thing: Int, other: Int) }

pub fn main() {
  let wibble = Wibble(1, 2)
  let wobble = Wibble(..wibble, thing: 1, thing: 2)
}
"
    );
}

#[test]
fn record_update_compatible_fields_wrong_variant() {
    assert_module_error!(
        r#"
pub type Wibble {
  A(a: Int, b: Int)
  B(a: Int, b: Int)
}

pub fn b_to_a(value: Wibble) {
  case value {
    A(..) -> value
    B(..) as b -> A(..b, b: 3)
  }
}
"#
    );
}

#[test]
fn record_update_compatible_fields_wrong_type() {
    assert_module_error!(
        r#"
pub type A {
  A(a: Int, b: Int)
}

pub type B {
  B(a: Int, b: Int)
}

pub fn b_to_a(value: B) {
  A(..value, b: 5)
}
"#
    );
}

#[test]
fn record_update_incompatible_but_linked_generics() {
    assert_module_error!(
        r#"
pub type Wibble(a) {
  Wibble(a: a, b: a)
}

pub fn b_to_a(value: Wibble(a)) -> Wibble(Int) {
  Wibble(..value, a: 5)
}
"#
    );
}

#[test]
// https://github.com/gleam-lang/gleam/issues/3879
fn inexhaustive_use_reports_error() {
    assert_error!(
        r#"
use [1, 2, 3] <- todo
todo
"#
    );
}

#[test]
fn out_of_range_erlang_float() {
    assert_error!(r#"1.8e308"#);
}

#[test]
fn out_of_range_erlang_float_in_pattern() {
    assert_error!(r#"let assert [1.8e308, b] = [x, y]"#);
}

#[test]
fn out_of_range_erlang_float_in_const() {
    assert_module_error!(r#"const x = 1.8e308"#);
}

#[test]
fn negative_out_of_range_erlang_float() {
    assert_error!(r#"-1.8e308"#);
}

#[test]
fn negative_out_of_range_erlang_float_in_pattern() {
    assert_error!(r#"let assert [-1.8e308, b] = [x, y]"#);
}

#[test]
fn negative_out_of_range_erlang_float_in_const() {
    assert_module_error!(r#"const x = -1.8e308"#);
}

#[test]
fn missing_case_body() {
    assert_error!("case True");
}

#[test]
fn suggest_wrapping_a_value_into_ok_if_types_match() {
    assert_module_error!(
        "
pub fn main() {
  case todo {
    1 -> Ok(2)
    _ -> 1
  }
}
"
    );
}

#[test]
fn suggest_wrapping_a_value_into_ok_if_types_match_2() {
    assert_module_error!(
        "
pub fn main() {
  wibble(1)
}

fn wibble(arg: Result(Int, String)) { todo }
"
    );
}

#[test]
fn suggest_wrapping_a_value_into_ok_if_types_match_with_block() {
    assert_module_error!(
        "
pub fn main() {
  case todo {
    1 -> Ok(2)
    _ -> {
      todo
      1
    }
  }
}
"
    );
}

#[test]
fn suggest_wrapping_a_value_into_ok_with_generic_type() {
    assert_module_error!(
        "
pub fn first(list: List(a)) -> Result(a, Nil) {
  case list {
    [] -> Error(Nil)
    [first, ..rest] -> first
  }
}
"
    );
}

#[test]
fn suggest_wrapping_a_value_into_ok_if_types_match_with_multiline_result_in_block() {
    assert_module_error!(
        "
pub fn main() {
  case todo {
    1 -> Ok(2)
    _ -> {
      todo
      1
      |> add_1
    }
  }
}

fn add_1(n: Int) { n + 1 }
"
    );
}

#[test]
fn suggest_wrapping_a_value_into_error_if_types_match() {
    assert_module_error!(
        "
pub fn main() {
  case todo {
    1 -> Error(1)
    _ -> 1
  }
}
"
    );
}

#[test]
fn suggest_wrapping_a_value_into_error_if_types_match_2() {
    assert_module_error!(
        "
pub fn main() {
    wibble(\"a\")
}

fn wibble(arg: Result(Int, String)) { todo }
"
    );
}

#[test]
fn suggest_wrapping_a_function_return_value_in_ok() {
    assert_module_error!(
        "
pub fn main() -> Result(Int, Bool) {
  1
}
"
    );
}

#[test]
fn suggest_wrapping_a_function_return_value_in_error() {
    assert_module_error!(
        "
pub fn main() -> Result(Int, Bool) {
  True
}
"
    );
}

#[test]
fn suggest_wrapping_a_use_returned_value_in_ok() {
    assert_module_error!(
        "
pub fn main() -> Result(Int, Bool) {
  use <- want_result
  1
}

pub fn want_result(wibble: fn() -> Result(Int, Bool)) {
  todo
}
"
    );
}

#[test]
fn suggest_wrapping_a_use_returned_value_in_error() {
    assert_module_error!(
        "
pub fn main() -> Result(Int, Bool) {
  use <- want_result
  False
}

pub fn want_result(wibble: fn() -> Result(Int, Bool)) {
  todo
}
"
    );
}

#[test]
// https://github.com/gleam-lang/gleam/issues/4195
fn let_assert_binding_cannot_be_used_in_panic_message() {
    assert_module_error!(
        r#"
pub fn main() {
  let assert Ok(message) = Error("Not Message") as { "Uh oh: " <> message }
}
"#
    );
}

#[test]
fn echo_followed_by_no_expression() {
    assert_error!("echo");
}

#[test]
fn echo_followed_by_no_expression_and_message() {
    assert_error!("echo as \"wibble\"");
}

#[test]
fn echo_followed_by_no_expression_and_invalid_message() {
    assert_error!("echo as 1");
}

#[test]
fn echo_followed_by_invalid_message() {
    assert_error!("echo 11 as { True || False }");
}

#[test]
fn echo_followed_by_no_expression_2() {
    assert_module_error!(
        r#"
  pub fn wibble(a) { a }

  pub fn main() {
    wibble(echo)
  }
"#
    );
}

#[test]
fn echo_followed_by_no_expression_3() {
    assert_module_error!(
        r#"
  pub fn main() {
    echo + 1
  }
"#
    );
}

#[test]
fn echo_followed_by_no_expression_4() {
    assert_module_error!(
        r#"
  pub fn main() {
    "wibble" <> echo
  }
"#
    );
}

#[test]
fn echo_followed_by_no_expression_5() {
    assert_module_error!(
        r#"
pub fn main() {
  panic as echo
}
"#
    );
}

#[test]
fn echo_followed_by_no_expression_6() {
    assert_module_error!(
        r#"
pub fn main() {
  [echo, 1, 2]
}
"#
    );
}
#[test]
fn echo_followed_by_no_expression_7() {
    assert_module_error!(
        r#"
pub fn main() {
  #(1, echo)
}
"#
    );
}

#[test]
fn echo_followed_by_no_expression_8() {
    assert_module_error!(
        r#"
pub fn main() {
  todo
  |> fn(_) { echo }
  |> todo
}
"#
    );
}

#[test]
fn echo_followed_by_no_expression_9() {
    assert_module_error!(
        r#"
pub fn main() {
  todo
  |> { echo }
  |> todo
}
"#
    );
}

#[test]
fn echo_followed_by_no_expression_10() {
    assert_module_error!(
        r#"
pub fn main() {
  echo
  |> todo
}
"#
    );
}

#[test]
fn function_that_does_not_exist_does_not_produce_error_for_labelled_args() {
    assert_module_error!(
        r#"
pub fn main() {
  // We only want to error on `wibble` since it doesn't exist, we don't want
  // an error on the label at this point!
  wibble(label: 1)
}
"#
    );
}

#[test]
fn constructor_that_does_not_exist_does_not_produce_error_for_labelled_args() {
    assert_module_error!(
        r#"
pub fn main() {
  // We only want to error on `Wibble` since it doesn't exist, we don't want
  // an error on the label at this point!
  Wibble(label: 1)
}
"#
    );
}

#[test]
fn float_operator_on_ints() {
    assert_error!("1 +. 2");
}

#[test]
fn float_operator_on_ints_2() {
    assert_error!("1 <. 2");
}

#[test]
fn int_operator_on_floats() {
    assert_error!("1.1 + 2.0");
}

#[test]
fn int_operator_on_floats_2() {
    assert_error!("1.1 > 2.0");
}

#[test]
fn add_on_strings() {
    assert_error!(r#""Hello, " + "Jak""#);
}

#[test]
fn fault_tolerant_list() {
    assert_module_error!(
        r#"
pub fn main() {
  [1, "a", 1.0, "a" + 1]
}
"#
    );
}

#[test]
fn fault_tolerant_list_tail() {
    assert_module_error!(
        r#"
pub fn main() {
  [1, "a", ..["a", "b"]]
}
"#
    );
}

#[test]
fn fault_tolerant_negate_bool() {
    assert_module_error!(
        r#"
pub fn main() {
  !!{ True || a }
}
"#
    );
}

#[test]
fn fault_tolerant_negate_int() {
    assert_module_error!(
        r#"
pub fn main() {
  --{ 1 + a }
}
"#
    );
}

#[test]
fn fault_tolerant_tuple() {
    assert_module_error!(
        r#"
pub fn main() {
  #(1, 1 + "a", not_in_scope)
}
"#
    );
}

#[test]
fn error_for_missing_type_parameters() {
    assert_module_error!(
        r#"
type Wibble(a)

type Wobble {
  Wobble(Wibble)
}
"#
    );
}

#[test]
fn double_assignment_in_bit_array() {
    assert_error!("let assert <<a as b>> = <<>>");
}

#[test]
fn negative_size_pattern() {
    assert_error!("let assert <<1:size(-1)>> = <<>>");
}

#[test]
fn zero_size_pattern() {
    assert_error!("let assert <<1:size(0)>> = <<>>");
}

// https://github.com/gleam-lang/gleam/issues/3253
#[test]
fn bit_array_using_pattern_variables() {
    assert_error!("let assert #(a, <<b:size(a)>>) = #(2, <<2:2>>)");
}

#[test]
fn bit_array_using_pattern_variables_from_other_bit_array() {
    assert_error!("let assert #(<<a>>, <<b:size(a)>>) = #(<<2>>, <<2:2>>)");
}

#[test]
fn non_utf8_string_assignment() {
    assert_error!(r#"let assert <<"Hello" as message:utf16>> = <<>>"#);
}

#[test]
fn shadowed_function_argument() {
    assert_module_error!(
        "
pub fn go(_x) {
  x + 1
}
"
    );
}

#[test]
fn shadowed_fn_argument() {
    assert_module_error!(
        "
pub fn go(x) {
  fn(_y) {
    y + x
  }
}
"
    );
}

#[test]
fn shadowed_let_variable() {
    assert_module_error!(
        "
pub fn go() {
  let _x = 1
  x + 1
}
"
    );
}

#[test]
fn shadowed_pattern_variable() {
    assert_module_error!(
        "
pub type Wibble {
  Wibble(Int)
}

pub fn go(x) {
  case x {
    Wibble(_y) -> y + 1
  }
}
"
    );
}

#[test]
fn do_not_suggest_ignored_variable_outside_of_current_scope() {
    assert_module_error!(
        "
pub fn go() {
  let _ = {
    let _y = 1 // <- this shouldn't be highlighted!
  }
  y
}
"
    );
}

// https://github.com/gleam-lang/gleam/issues/4693
#[test]
fn pattern_with_incorrect_arity() {
    assert_module_error!(
        "
pub type Pokemon { Pokemon(name: String, id: Int) }

pub fn main() {
  case todo {
    Pokemon(name:) -> todo
  }
}
"
    );
}

// https://github.com/gleam-lang/gleam/issues/3884
#[test]
fn show_only_missing_labels() {
    assert_module_error!(
        "
fn wibble(a a: Int, b b: Float, c c: String) {
    todo
}

pub fn wobble() {
    wibble(1, 2.0)
}
"
    );
}
#[test]
fn native_endianness_javascript_target() {
    assert_js_module_error!(
        "
pub fn main() {
  let assert <<a:native>> = <<10>>
}
"
    );
}

#[test]
fn utf8_codepoint_javascript_target() {
    assert_js_module_error!(
        "
pub fn main() {
  let assert <<a:utf8_codepoint>> = <<10>>
}
"
    );
}

#[test]
fn utf16_codepoint_javascript_target() {
    assert_js_module_error!(
        "
pub fn main() {
  let assert <<a:utf16_codepoint>> = <<10>>
}
"
    );
}

#[test]
fn utf32_codepoint_javascript_target() {
    assert_js_module_error!(
        "
pub fn main() {
  let assert <<a:utf32_codepoint>> = <<10>>
}
"
    );
}

#[test]
fn private_opaque_type() {
    assert_module_error!(
        "
opaque type Wibble {
  Wobble
}
"
    );
}

#[test]
fn src_importing_dev_dependency() {
    assert_module_error!(
        ("dev_dependency", "some_module", "pub fn main() { Nil }"),
        "
import some_module

pub fn main() {
  some_module.main()
}
"
    );
}

#[test]
fn missing_type_constructor_arguments_in_type_annotation_1() {
    assert_module_error!("pub fn main() -> Result() {}");
}

#[test]
fn missing_type_constructor_arguments_in_type_annotation_2() {
    assert_module_error!(
        "pub fn main() {
  let a: Result() = todo
}"
    );
}

#[test]
fn type_used_as_a_constructor_1() {
    assert_module_error!("pub fn main() -> Int() {}");
}

#[test]
fn type_used_as_a_constructor_2() {
    assert_module_error!(
        "pub fn main() {
  let a: Int() = todo
}"
    );
}

#[test]
fn type_used_as_a_constructor_with_more_arguments() {
    assert_module_error!(
        "pub fn main() {
  let a: Int(Int, String) = todo
}"
    );
}

#[test]
fn remembering_record_field_when_type_checking_fails() {
    assert_module_error!(
        r#"pub type Wibble {
  Wibble(x: Int, f: fn(Wobble) -> Int)
}

pub fn wibble() {
  Wibble(1, fn(_) { 2 })
}

pub fn wobble(wibble: Wibble) {
  wibble.f
}

pub fn woo(wibble: Wibble) {
  Wibble(..wibble, x: 1)
}"#
    );
}

#[test]
fn external_annotation_on_custom_type_with_constructors() {
    assert_module_error!(
        r#"
@external(erlang, "gleam_stdlib", "dict")
pub type Dict(key, value) {
  Dict(pairs: List(#(key, value)))
}
"#
    );
}

#[test]
fn generic_unlabelled_field_in_updated_record_wrong_type() {
    assert_module_error!(
        "
pub type Wibble(a) {
  Wibble(a, b: Int, c: a)
}

pub fn main() {
  let w = Wibble(1, 2, 3)
  Wibble(..w, c: False)
}
"
    );
}
