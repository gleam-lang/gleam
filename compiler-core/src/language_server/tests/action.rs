use itertools::Itertools;
use lsp_types::{
    CodeActionContext, CodeActionParams, PartialResultParams, Position, Range, Url,
    WorkDoneProgressParams,
};

use super::*;

fn code_actions(tester: TestProject<'_>, range: Range) -> Option<Vec<lsp_types::CodeAction>> {
    let position = Position {
        line: 0,
        character: 0,
    };

    tester.at(position, |engine, params, _| {
        let params = CodeActionParams {
            text_document: params.text_document,
            range,
            context: CodeActionContext::default(),
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        };
        engine.code_actions(params).result.unwrap()
    })
}

fn actions_with_title(
    titles: Vec<&str>,
    tester: TestProject<'_>,
    range: Range,
) -> Vec<lsp_types::CodeAction> {
    code_actions(tester, range)
        .into_iter()
        .flatten()
        .filter(|action| titles.contains(&action.title.as_str()))
        .collect_vec()
}

fn apply_code_action(title: &str, tester: TestProject<'_>, range: Range) -> String {
    let src = tester.src;
    let titles = vec![title];
    let changes = actions_with_title(titles, tester, range)
        .pop()
        .expect("No action with the given title")
        .edit
        .expect("No workspace edit found")
        .changes
        .expect("No text edit found");
    apply_code_edit(src, changes)
}

fn apply_code_edit(src: &str, changes: HashMap<Url, Vec<lsp_types::TextEdit>>) -> String {
    let mut result = src.to_string();
    for (_, change) in changes {
        result = super::apply_code_edit(result.as_str(), change);
    }
    result.to_string()
}

const REMOVE_UNUSED_IMPORTS: &str = "Remove unused imports";
const REMOVE_REDUNDANT_TUPLES: &str = "Remove redundant tuples";
const CONVERT_TO_CASE: &str = "Convert to case";
const USE_LABEL_SHORTHAND_SYNTAX: &str = "Use label shorthand syntax";
const FILL_LABELS: &str = "Fill labels";
const ASSIGN_UNUSED_RESULT: &str = "Assign unused Result value to `_`";
const ADD_MISSING_PATTERNS: &str = "Add missing patterns";
const ADD_ANNOTATION: &str = "Add type annotation";
const ADD_ANNOTATIONS: &str = "Add type annotations";
const DESUGAR_USE_EXPRESSION: &str = "Convert from `use`";
const CONVERT_TO_USE: &str = "Convert to `use`";
const EXTRACT_VARIABLE: &str = "Extract variable";

macro_rules! assert_code_action {
    ($title:expr, $code:literal, $range:expr $(,)?) => {
        let project = TestProject::for_source($code);
        assert_code_action!($title, project, $range);
    };

    ($title:expr, $project:expr, $range:expr $(,)?) => {
        let src = $project.src;
        let range = $range.find_range(src);
        let result = apply_code_action($title, $project, range);
        let output = format!(
            "----- BEFORE ACTION\n{}\n\n----- AFTER ACTION\n{}",
            hover::show_hover(src, range, range.end),
            result
        );
        insta::assert_snapshot!(insta::internals::AutoName, output, src);
    };
}

macro_rules! assert_no_code_actions {
    ($title:ident $(| $titles:ident)*, $code:literal, $range:expr $(,)?) => {
        let project = TestProject::for_source($code);
        assert_no_code_actions!($title $(| $titles)*, project, $range);
    };

    ($title:ident $(| $titles:ident)*, $project:expr, $range:expr $(,)?) => {
        let src = $project.src;
        let range = $range.find_range(src);
        let all_titles = vec![$title $(, $titles)*];
        let expected: Vec<lsp_types::CodeAction> = vec![];
        let result = actions_with_title(all_titles, $project, range);
        assert_eq!(expected, result);
    };
}

#[test]
fn test_remove_unused_simple() {
    let src = "
// test
import // comment
list as lispy
import result
import option

pub fn main() {
  result.is_ok
}
";

    assert_code_action!(
        REMOVE_UNUSED_IMPORTS,
        TestProject::for_source(src)
            .add_hex_module("list", "")
            .add_hex_module("result", "")
            .add_hex_module("option", ""),
        find_position_of("// test").select_until(find_position_of("option")),
    );
}

#[test]
fn test_remove_unused_start_of_file() {
    let src = "import option
import result

pub fn main() {
  result.is_ok
}
";
    assert_code_action!(
        REMOVE_UNUSED_IMPORTS,
        TestProject::for_source(src)
            .add_hex_module("option", "")
            .add_hex_module("result", ""),
        find_position_of("import").select_until(find_position_of("pub")),
    );
}

#[test]
fn test_remove_unused_alias() {
    let src = "
// test
import result.{is_ok} as res
import option

pub fn main() {
  is_ok
}
";
    assert_code_action!(
        REMOVE_UNUSED_IMPORTS,
        TestProject::for_source(src)
            .add_hex_module("result", "pub fn is_ok() {}")
            .add_hex_module("option", ""),
        find_position_of("// test").select_until(find_position_of("pub")),
    );
}

#[test]
fn test_remove_redundant_tuple_in_case_subject_simple() {
    assert_code_action!(
        REMOVE_REDUNDANT_TUPLES,
        "pub fn main() {
  case #(1) { #(a) -> 0 }
  case #(1, 2) { #(a, b) -> 0 }
}",
        find_position_of("case").select_until(find_position_of("#(1, 2)").under_last_char())
    );
}

#[test]
fn test_remove_redundant_tuple_with_catch_all_pattern() {
    assert_code_action!(
        REMOVE_REDUNDANT_TUPLES,
        "pub fn main() {
  case #(1, 2) {
    #(1, 2) -> 0
    _ -> 1
  }
}",
        find_position_of("case").select_until(find_position_of("#(1, 2)").under_last_char())
    );
}

#[test]
fn test_remove_multiple_redundant_tuple_with_catch_all_pattern() {
    assert_code_action!(
        REMOVE_REDUNDANT_TUPLES,
        "pub fn main() {
  case #(1, 2), #(3, 4) {
    #(2, 2), #(2, 2) -> 0
    #(1, 2), _ -> 0
    _, #(1, 2) -> 0
    _, _ -> 1
  }
}",
        find_position_of("case").select_until(find_position_of("#(3, 4)"))
    );
}

#[test]
fn test_remove_redundant_tuple_in_case_subject_nested() {
    assert_code_action!(
        REMOVE_REDUNDANT_TUPLES,
        "pub fn main() {
  case #(case #(0) { #(a) -> 0 }) { #(b) -> 0 }
}",
        find_position_of("case").select_until(find_position_of("#(b)"))
    );
}

#[test]
fn test_remove_redundant_tuple_in_case_retain_extras() {
    assert_code_action!(
        REMOVE_REDUNDANT_TUPLES,
        "
pub fn main() {
  case
    #(
      // first comment
      1,
      // second comment
      2,
      3 // third comment before comma

      ,

      // fourth comment after comma

    )
  {
    #(
      // first comment
      a,
      // second comment
      b,
      c // third comment before comma

      ,

      // fourth comment after comma

    ) -> 0
  }
}
",
        find_position_of("#").select_until(find_position_of("// first"))
    );
}

#[test]
fn test_remove_redundant_tuple_in_case_subject_ignore_empty_tuple() {
    assert_no_code_actions!(
        REMOVE_REDUNDANT_TUPLES,
        "
pub fn main() {
  case #() { #() -> 0 }
}
",
        find_position_of("case").select_until(find_position_of("0"))
    );
}

#[test]
fn test_remove_redundant_tuple_in_case_subject_only_safe_remove() {
    assert_code_action!(
        REMOVE_REDUNDANT_TUPLES,
        "
pub fn main() {
  case #(0), #(1) {
    #(1), #(b) -> 0
    a, #(0) -> 1 // The first of this clause is not a tuple
    #(a), #(b) -> 2
  }
}
",
        find_position_of("#(0)").select_until(find_position_of("#(1)"))
    );
}

#[test]
fn rename_invalid_const() {
    assert_code_action!(
        "Rename to my_invalid_constant",
        "const myInvalid_Constant = 42",
        find_position_of("_Constant").to_selection(),
    );
}

#[test]
fn rename_invalid_parameter() {
    assert_code_action!(
        "Rename to num_a",
        "fn add(numA: Int, num_b: Int) { numA + num_b }",
        find_position_of("numA").to_selection()
    );
}

#[test]
fn rename_invalid_parameter_name2() {
    assert_code_action!(
        "Rename to param_name",
        "fn pass(label paramName: Bool) { paramName }",
        find_position_of("paramName").to_selection()
    );
}

#[test]
fn rename_invalid_parameter_name3() {
    assert_code_action!(
        "Rename to num_a",
        "pub fn main() {
    let add = fn(numA: Int, num_b: Int) { numA + num_b }
}",
        find_position_of("let add").select_until(find_position_of("num_b"))
    );
}

#[test]
fn rename_invalid_parameter_discard() {
    assert_code_action!(
        "Rename to _ignore_me",
        "fn ignore(_ignoreMe: Bool) { 98 }",
        find_position_of("ignore").select_until(find_position_of("98"))
    );
}

#[test]
fn rename_invalid_parameter_discard_name2() {
    assert_code_action!(
        "Rename to _ignore_me",
        "fn ignore(labelled_discard _ignoreMe: Bool) { 98 }",
        find_position_of("ignore").select_until(find_position_of("98"))
    );
}

#[test]
fn rename_invalid_parameter_discard_name3() {
    assert_code_action!(
        "Rename to _ignore_me",
        "pub fn main() {
    let ignore = fn(_ignoreMe: Bool) { 98 }
}",
        find_position_of("ignore").select_until(find_position_of("98"))
    );
}

#[test]
fn rename_invalid_parameter_label() {
    assert_code_action!(
        "Rename to this_is_a_label",
        "fn func(thisIsALabel param: Int) { param }",
        find_position_of("thisIs").select_until(find_position_of("Int"))
    );
}

#[test]
fn rename_invalid_parameter_label2() {
    assert_code_action!(
        "Rename to this_is_a_label",
        "fn ignore(thisIsALabel _ignore: Int) { 25 }",
        find_position_of("thisIs").under_char('i').to_selection()
    );
}

#[test]
fn rename_invalid_constructor() {
    assert_code_action!(
        "Rename to TheConstructor",
        "type MyType { The_Constructor(Int) }",
        find_position_of("The_").under_char('h').to_selection(),
    );
}

#[test]
fn rename_invalid_constructor_arg() {
    assert_code_action!(
        "Rename to inner_int",
        "type IntWrapper { IntWrapper(innerInt: Int) }",
        find_position_of("IntWrapper")
            .nth_occurrence(2)
            .select_until(find_position_of(": Int"))
    );
}

#[test]
fn rename_invalid_custom_type() {
    assert_code_action!(
        "Rename to BoxedValue",
        "type Boxed_value { Box(Int) }",
        find_position_of("Box").select_until(find_position_of("_value"))
    );
}

#[test]
fn rename_invalid_type_alias() {
    assert_code_action!(
        "Rename to FancyBool",
        "type Fancy_Bool = Bool",
        find_position_of("Fancy")
            .under_char('a')
            .select_until(find_position_of("="))
    );
}

#[test]
fn rename_invalid_function() {
    assert_code_action!(
        "Rename to do_stuff",
        "fn doStuff() {}",
        find_position_of("fn").select_until(find_position_of("{}"))
    );
}

#[test]
fn rename_invalid_variable() {
    assert_code_action!(
        "Rename to the_answer",
        "pub fn main() {
    let theAnswer = 42
}",
        find_position_of("theAnswer").select_until(find_position_of("Answer"))
    );
}

#[test]
fn rename_invalid_variable_discard() {
    assert_code_action!(
        "Rename to _boring_number",
        "pub fn main() {
    let _boringNumber = 72
}",
        find_position_of("let").select_until(find_position_of("72"))
    );
}

#[test]
fn rename_invalid_use() {
    assert_code_action!(
        "Rename to use_var",
        "fn use_test(f) { f(Nil) }
pub fn main() {use useVar <- use_test()}",
        find_position_of("use")
            .nth_occurrence(2)
            .select_until(find_position_of("use_test()"))
    );
}

#[test]
fn rename_invalid_use_discard() {
    assert_code_action!(
        "Rename to _discard_var",
        "fn use_test(f) { f(Nil) }
pub fn main() {use _discardVar <- use_test()}",
        find_position_of("_discardVar")
            .under_last_char()
            .to_selection()
    );
}

#[test]
fn rename_invalid_pattern_assignment() {
    assert_code_action!(
        "Rename to the_answer",
        "pub fn main() {
    let assert 42 as theAnswer = 42
}",
        find_position_of("let").select_until(find_position_of("= 42"))
    );
}

#[test]
fn rename_invalid_list_pattern() {
    assert_code_action!(
        "Rename to the_element",
        "pub fn main() {
    let assert [theElement] = [9.4]
}",
        find_position_of("assert").select_until(find_position_of("9.4"))
    );
}

#[test]
fn rename_invalid_list_pattern_discard() {
    assert_code_action!(
        "Rename to _elem_one",
        "pub fn main() {
    let assert [_elemOne] = [False]
}",
        find_position_of("[_elemOne]")
            .under_char('O')
            .to_selection()
    );
}

#[test]
fn rename_invalid_constructor_pattern() {
    assert_code_action!(
        "Rename to inner_value",
        "pub type Box { Box(Int) }
pub fn main() {
    let Box(innerValue) = Box(203)
}",
        find_position_of("innerValue").to_selection()
    );
}

#[test]
fn rename_invalid_constructor_pattern_discard() {
    assert_code_action!(
        "Rename to _ignored_inner",
        "pub type Box { Box(Int) }
pub fn main() {
    let Box(_ignoredInner) = Box(203)
}",
        find_position_of("_").select_until(find_position_of("203"))
    );
}

#[test]
fn rename_invalid_tuple_pattern() {
    assert_code_action!(
        "Rename to second_value",
        "pub fn main() {
    let #(a, secondValue) = #(1, 2)
}",
        find_position_of("secondValue")
            .select_until(find_position_of("secondValue").under_char('n'))
    );
}

#[test]
fn rename_invalid_tuple_pattern_discard() {
    assert_code_action!(
        "Rename to _second_value",
        "pub fn main() {
    let #(a, _secondValue) = #(1, 2)
}",
        find_position_of("_secondValue")
            .under_char('_')
            .select_until(find_position_of("#(1, 2)"))
    );
}

#[test]
fn rename_invalid_bit_array_pattern() {
    assert_code_action!(
        "Rename to bit_value",
        "pub fn main() {
    let assert <<bitValue>> = <<73>>
}",
        find_position_of("<<").select_until(find_position_of(">>"))
    );
}

#[test]
fn rename_invalid_bit_array_pattern_discard() {
    assert_code_action!(
        "Rename to _i_dont_care",
        "pub fn main() {
    let assert <<_iDontCare>> = <<97>>
}",
        find_position_of("<<").select_until(find_position_of("Care"))
    );
}

#[test]
fn rename_invalid_string_prefix_pattern() {
    assert_code_action!(
        "Rename to cool_suffix",
        r#"pub fn main() {
    let assert "prefix" <> coolSuffix = "prefix-suffix"
}"#,
        find_position_of("<>").select_until(find_position_of("-suffix"))
    );
}

#[test]
fn rename_invalid_string_prefix_pattern_discard() {
    assert_code_action!(
        "Rename to _boring_suffix",
        r#"pub fn main() {
    let assert "prefix" <> _boringSuffix = "prefix-suffix"
}"#,
        find_position_of("<>").select_until(find_position_of("Suffix"))
    );
}

#[test]
fn rename_invalid_string_prefix_pattern_alias() {
    assert_code_action!(
        "Rename to the_prefix",
        r#"pub fn main() {
    let assert "prefix" as thePrefix <> _suffix = "prefix-suffix"
}"#,
        find_position_of("prefix").select_until(find_position_of("-suffix"))
    );
}

#[test]
fn rename_invalid_case_variable() {
    assert_code_action!(
        "Rename to twenty_one",
        "pub fn main() {
    case 21 { twentyOne -> {Nil} }
}",
        find_position_of("case").select_until(find_position_of("Nil"))
    );
}

#[test]
fn rename_invalid_case_variable_discard() {
    assert_code_action!(
        "Rename to _twenty_one",
        "pub fn main() {
    case 21 { _twentyOne -> {Nil} }
}",
        find_position_of("21").select_until(find_position_of("->"))
    );
}

#[test]
fn rename_invalid_type_parameter_name() {
    assert_code_action!(
        "Rename to inner_type",
        "type Wrapper(innerType) {}",
        find_position_of("innerType").select_until(find_position_of(")"))
    );
}

#[test]
fn rename_invalid_type_alias_parameter_name() {
    assert_code_action!(
        "Rename to phantom_type",
        "type Phantom(phantomType) = Int",
        find_position_of("phantomType").select_until(find_position_of(")"))
    );
}

#[test]
fn rename_invalid_function_type_parameter_name() {
    assert_code_action!(
        "Rename to some_type",
        "fn identity(value: someType) { value }",
        find_position_of("someType").select_until(find_position_of(")"))
    );
}

#[test]
fn test_convert_assert_result_to_case() {
    assert_code_action!(
        CONVERT_TO_CASE,
        "pub fn main() {
  let assert Ok(value) = Ok(1)
}",
        find_position_of("assert").select_until(find_position_of("assert").under_char('r')),
    );
}

#[test]
fn test_convert_let_assert_to_case_indented() {
    assert_code_action!(
        CONVERT_TO_CASE,
        "pub fn main() {
  {
    let assert Ok(value) = Ok(1)
  }
}",
        find_position_of("Ok").to_selection()
    );
}

#[test]
fn test_convert_let_assert_to_case_multi_variables() {
    assert_code_action!(
        CONVERT_TO_CASE,
        "pub fn main() {
  let assert [var1, var2, _var3, var4] = [1, 2, 3, 4]
}",
        find_position_of("var1").select_until(find_position_of("_var").under_last_char())
    );
}

#[test]
fn test_convert_let_assert_to_case_discard() {
    assert_code_action!(
        CONVERT_TO_CASE,
        "pub fn main() {
  let assert [_elem] = [6]
}",
        find_position_of("assert").select_until(find_position_of("[6]").under_last_char()),
    );
}

#[test]
fn test_convert_let_assert_to_case_no_variables() {
    assert_code_action!(
        CONVERT_TO_CASE,
        "pub fn main() {
  let assert [] = []
}",
        find_position_of("[]").to_selection(),
    );
}

#[test]
fn test_convert_let_assert_alias_to_case() {
    assert_code_action!(
        CONVERT_TO_CASE,
        "pub fn main() {
  let assert 10 as ten = 10
}",
        find_position_of("as").select_until(find_position_of("ten")),
    );
}

#[test]
fn test_convert_let_assert_tuple_to_case() {
    assert_code_action!(
        CONVERT_TO_CASE,
        "pub fn main() {
   let assert #(first, 10, third) = #(5, 10, 15)
}
",
        find_position_of("let").to_selection(),
    );
}

#[test]
fn test_convert_let_assert_bit_array_to_case() {
    assert_code_action!(
        CONVERT_TO_CASE,
        "pub fn main() {
  let assert <<bits1, bits2>> = <<73, 98>>
}",
        find_position_of("bits").select_until(find_position_of("2")),
    );
}

#[test]
fn test_convert_let_assert_string_prefix_to_case() {
    assert_code_action!(
        CONVERT_TO_CASE,
        r#"pub fn main() {
  let assert "_" <> thing = "_Hello"
}"#,
        find_position_of("_").to_selection()
    );
}

#[test]
fn test_convert_let_assert_string_prefix_pattern_alias_to_case() {
    assert_code_action!(
        CONVERT_TO_CASE,
        r#"pub fn main() {
    let assert "123" as one_two_three <> rest = "123456"
}"#,
        find_position_of("123").select_until(find_position_of("123456")),
    );
}

#[test]
fn test_convert_inner_let_assert_to_case() {
    assert_code_action!(
        CONVERT_TO_CASE,
        r#"pub fn main() {
    let assert [wibble] = {
        let assert Ok(wobble) = {
            Ok(1)
        }
        [wobble]
    }
}"#,
        find_position_of("wobble").under_char('l').to_selection()
    );
}

#[test]
fn test_convert_outer_let_assert_to_case() {
    assert_code_action!(
        CONVERT_TO_CASE,
        r#"pub fn main() {
    let assert [wibble] = {
        let assert Ok(wobble) = {
            Ok(1)
        }
        [wobble]
    }
}"#,
        find_position_of("wibble")
            .under_char('i')
            .select_until(find_position_of("= {")),
    );
}

#[test]
fn test_convert_let_assert_with_message_to_case() {
    assert_code_action!(
        CONVERT_TO_CASE,
        r#"
pub fn expect(value, message) {
  let assert Ok(inner) = value as message
  inner
}
"#,
        find_position_of("assert").select_until(find_position_of("=")),
    );
}

#[test]
fn test_convert_assert_custom_type_with_label_shorthands_to_case() {
    assert_code_action!(
        CONVERT_TO_CASE,
        "
pub type Wibble { Wibble(arg: Int, arg2: Float) }
pub fn main() {
  let assert Wibble(arg2:, ..) = Wibble(arg: 1, arg2: 1.0)
}
",
        find_position_of("arg2:,").select_until(find_position_of("1.0")),
    );
}

#[test]
fn label_shorthand_action_works_on_labelled_call_args() {
    assert_code_action!(
        USE_LABEL_SHORTHAND_SYNTAX,
        r#"
pub fn main() {
    let arg1 = 1
    let arg2 = 2
    wibble(arg2: arg2, arg1: arg1)
}

pub fn wibble(arg1 arg1, arg2 arg2) { Nil }
"#,
        find_position_of("wibble")
            .under_char('i')
            .select_until(find_position_of("arg1: arg1")),
    );
}

#[test]
fn label_shorthand_action_works_on_labelled_constructor_call_args() {
    assert_code_action!(
        USE_LABEL_SHORTHAND_SYNTAX,
        r#"
pub fn main() {
    let arg1 = 1
    let arg2 = 2
    Wibble(arg2: arg2, arg1: arg1)
}

pub type Wibble { Wibble(arg1: Int, arg2: Int) }
"#,
        find_position_of("Wibble").select_until(find_position_of("arg1: arg1").under_char(':')),
    );
}

#[test]
fn label_shorthand_action_only_applies_to_selected_args() {
    assert_code_action!(
        USE_LABEL_SHORTHAND_SYNTAX,
        r#"
pub fn main() {
    let arg1 = 1
    let arg2 = 2
    Wibble(arg2: arg2, arg1: arg1)
}

pub type Wibble { Wibble(arg1: Int, arg2: Int) }
"#,
        find_position_of("Wibble").select_until(find_position_of("arg2: arg2").under_char(':')),
    );
}

#[test]
fn label_shorthand_action_works_on_labelled_update_call_args() {
    assert_code_action!(
        USE_LABEL_SHORTHAND_SYNTAX,
        r#"
pub fn main() {
    let arg1 = 1
    Wibble(..todo, arg1: arg1)
}

pub type Wibble { Wibble(arg1: Int, arg2: Int) }
"#,
        find_position_of("..todo").select_until(find_position_of("arg1: arg1").under_last_char()),
    );
}

#[test]
fn label_shorthand_action_works_on_labelled_pattern_call_args() {
    assert_code_action!(
        USE_LABEL_SHORTHAND_SYNTAX,
        r#"
pub fn main() {
    let Wibble(arg1: arg1, arg2: arg2) = todo
    arg1 + arg2
}

pub type Wibble { Wibble(arg1: Int, arg2: Int) }
"#,
        find_position_of("let").select_until(find_position_of("todo").under_last_char()),
    );
}

#[test]
fn label_shorthand_action_doesnt_come_up_for_arguments_with_different_label() {
    assert_no_code_actions!(
        USE_LABEL_SHORTHAND_SYNTAX,
        r#"
pub fn main() {
  let Wibble(arg1: arg_1, arg2: arg_2) = todo
  arg_1 + arg_2
}

pub type Wibble { Wibble(arg1: Int, arg2: Int) }
"#,
        find_position_of("arg_1").select_until(find_position_of("arg_2").under_last_char())
    );
}

#[test]
fn fill_in_labelled_args_only_works_if_function_has_no_explicit_arguments_yet() {
    assert_no_code_actions!(
        FILL_LABELS,
        r#"
pub fn main() {
  wibble(1,)
}

pub fn wibble(arg1 arg1, arg2 arg2) { Nil }
 "#,
        find_position_of("wibble(").under_char('b').to_selection(),
    );
}

#[test]
fn fill_in_labelled_args_only_works_if_function_has_no_explicit_arguments_yet_2() {
    assert_no_code_actions!(
        FILL_LABELS,
        r#"
pub fn main() {
  wibble(arg2: 1)
}

pub fn wibble(arg1 arg1, arg2 arg2) { Nil }
 "#,
        find_position_of("wibble(").to_selection(),
    );
}

#[test]
fn fill_in_labelled_args_works_with_regular_function() {
    assert_code_action!(
        FILL_LABELS,
        r#"
pub fn main() {
  wibble()
}

pub fn wibble(arg1 arg1, arg2 arg2) { Nil }
 "#,
        find_position_of("wibble(").to_selection(),
    );
}

#[test]
fn fill_in_labelled_args_works_with_record_constructor() {
    assert_code_action!(
        FILL_LABELS,
        r#"
pub fn main() {
  Wibble()
}

pub type Wibble { Wibble(arg1: Int, arg2: String) }
 "#,
        find_position_of("Wibble").select_until(find_position_of("Wibble()").under_last_char()),
    );
}

#[test]
fn fill_in_labelled_args_works_with_pipes() {
    assert_code_action!(
        FILL_LABELS,
        r#"
pub fn main() {
  1 |> wibble()
}

pub fn wibble(arg1 arg1, arg2 arg2) { Nil }
 "#,
        find_position_of("wibble()")
            .under_last_char()
            .to_selection(),
    );
}

#[test]
fn fill_in_labelled_args_works_with_pipes_2() {
    assert_code_action!(
        FILL_LABELS,
        r#"
pub fn main() {
  1 |> wibble()
}

pub fn wibble(not_labelled, arg1 arg1, arg2 arg2) { Nil }
 "#,
        find_position_of("wibble()")
            .under_last_char()
            .to_selection(),
    );
}

#[test]
fn fill_in_labelled_args_works_with_use() {
    assert_code_action!(
        FILL_LABELS,
        r#"
pub fn main() {
  use <- wibble()
}

pub fn wibble(arg1 arg1, arg2 arg2) { Nil }
 "#,
        find_position_of("wibble(").select_until(find_position_of("wibble()").under_last_char()),
    );
}

#[test]
fn fill_in_labelled_args_selects_innermost_function() {
    assert_code_action!(
        FILL_LABELS,
        r#"
pub fn main() {
  wibble(
    wibble()
  )
}

pub fn wibble(arg1 arg1, arg2 arg2) { Nil }
 "#,
        find_position_of("wibble()")
            .under_last_char()
            .to_selection(),
    );
}

#[test]
fn use_label_shorthand_works_for_nested_calls() {
    assert_code_action!(
        USE_LABEL_SHORTHAND_SYNTAX,
        r#"
pub fn wibble(arg arg: Int) -> Int { arg }

pub fn main() {
  let arg = 1
  wibble(wibble(arg: arg))
}
 "#,
        find_position_of("main").select_until(find_position_of("}").nth_occurrence(2)),
    );
}

#[test]
fn use_label_shorthand_works_for_nested_record_updates() {
    assert_code_action!(
        USE_LABEL_SHORTHAND_SYNTAX,
        r#"
pub type Wibble { Wibble(arg: Int, arg2: Wobble) }
pub type Wobble { Wobble(arg: Int, arg2: String) }

pub fn main() {
  let arg = 1
  let arg2 = "a"
  Wibble(..todo, arg2: Wobble(arg: arg, arg2: arg2))
}
 "#,
        find_position_of("todo").select_until(find_position_of("arg2: arg2")),
    );
}

#[test]
fn use_label_shorthand_works_for_nested_patterns() {
    assert_code_action!(
        USE_LABEL_SHORTHAND_SYNTAX,
        r#"
pub type Wibble { Wibble(arg: Int, arg2: Wobble) }
pub type Wobble { Wobble(arg: Int, arg2: String) }

pub fn main() {
  let Wibble(arg2: Wobble(arg: arg, arg2: arg2), ..) = todo
}
 "#,
        find_position_of("main").select_until(find_position_of("todo")),
    );
}

#[test]
fn use_label_shorthand_works_for_alternative_patterns() {
    assert_code_action!(
        USE_LABEL_SHORTHAND_SYNTAX,
        r#"
pub type Wibble { Wibble(arg: Int, arg2: String) }

pub fn main() {
  case Wibble(1, "wibble") {
    Wibble(arg2: arg2, ..) | Wibble(arg: 1, arg2: arg2) -> todo
  }
}
 "#,
        find_position_of("main").select_until(find_position_of("todo")),
    );
}

#[test]
fn test_assign_unused_result() {
    assert_code_action!(
        ASSIGN_UNUSED_RESULT,
        r#"
pub fn main() {
    Ok(0)
    Nil
}
"#,
        find_position_of("Ok").select_until(find_position_of("(0)")),
    );
}

#[test]
fn test_assign_unused_result_in_block() {
    assert_code_action!(
        ASSIGN_UNUSED_RESULT,
        r#"
pub fn main() {
    {
        Ok(0)
        Nil
    }
    Nil
}
"#,
        find_position_of("Ok").select_until(find_position_of("(0)")),
    );
}

#[test]
fn test_assign_unused_result_on_block_start() {
    assert_code_action!(
        ASSIGN_UNUSED_RESULT,
        r#"
pub fn main() {
    {
        Ok(0)
        Ok(0)
    }
    Nil
}
"#,
        find_position_of("{").nth_occurrence(2).to_selection()
    );
}

#[test]
fn test_assign_unused_result_on_block_end() {
    assert_code_action!(
        ASSIGN_UNUSED_RESULT,
        r#"
pub fn main() {
    {
        Ok(0)
        Ok(0)
    }
    Nil
}
"#,
        find_position_of("}").to_selection()
    );
}

#[test]
#[should_panic(expected = "No action with the given title")]
fn test_assign_unused_result_inside_block() {
    assert_code_action!(
        ASSIGN_UNUSED_RESULT,
        r#"
pub fn main() {
    {
        Nil
        Ok(1)
    }
}
"#,
        find_position_of("Ok").select_until(find_position_of("(1)"))
    );
}

#[test]
fn test_assign_unused_result_only_first_action() {
    assert_code_action!(
        ASSIGN_UNUSED_RESULT,
        r#"
pub fn main() {
    Ok(0)
    Ok(1)
    Nil
}
"#,
        find_position_of("Ok").select_until(find_position_of("(0)"))
    );
}

#[test]
#[should_panic(expected = "No action with the given title")]
fn test_assign_unused_result_not_on_return_value() {
    assert_code_action!(
        ASSIGN_UNUSED_RESULT,
        r#"
pub fn main() {
    Ok(0)
}
"#,
        find_position_of("Ok").select_until(find_position_of("(0)"))
    );
}

#[test]
#[should_panic(expected = "No action with the given title")]
fn test_assign_unused_result_not_on_return_value_in_block() {
    assert_code_action!(
        ASSIGN_UNUSED_RESULT,
        r#"
pub fn main() {
    let _ = {
        Ok(0)
    }
    Nil
}"#,
        find_position_of("Ok").select_until(find_position_of("(0)"))
    );
}

#[test]
fn test_import_module_from_function() {
    let src = "
pub fn main() {
  result.is_ok()
}
";

    assert_code_action!(
        "Import `result`",
        TestProject::for_source(src).add_hex_module("result", "pub fn is_ok() {}"),
        find_position_of("result").select_until(find_position_of("."))
    );
}

#[test]
fn test_import_path_module_from_function() {
    let src = r#"
pub fn main() {
  io.println("Hello, world!")
}
"#;

    assert_code_action!(
        "Import `gleam/io`",
        TestProject::for_source(src)
            .add_hex_module("gleam/io", "pub fn println(message: String) {}"),
        find_position_of("io").select_until(find_position_of("."))
    );
}

#[test]
fn test_import_module_from_type() {
    let src = "type Wobble = wibble.Wubble";

    assert_code_action!(
        "Import `mod/wibble`",
        TestProject::for_source(src).add_hex_module("mod/wibble", "pub type Wubble { Wubble }"),
        find_position_of("wibble").select_until(find_position_of("."))
    );
}

#[test]
fn test_import_module_from_constructor() {
    let src = "
pub fn main() {
  let value = values.Value(10)
}
";

    assert_code_action!(
        "Import `values`",
        TestProject::for_source(src).add_hex_module("values", "pub type Value { Value(Int) }"),
        find_position_of("values").select_until(find_position_of("."))
    );
}

#[test]
fn test_rename_module_for_imported() {
    let src = r#"
import gleam/io

pub fn main() {
  i.println("Hello, world!")
}
"#;

    assert_code_action!(
        "Did you mean `io`",
        TestProject::for_source(src)
            .add_hex_module("gleam/io", "pub fn println(message: String) {}"),
        find_position_of("i.").select_until(find_position_of("println"))
    );
}

#[test]
fn test_import_similar_module() {
    let src = "
pub fn main() {
  reult.is_ok()
}
";

    assert_code_action!(
        "Import `result`",
        TestProject::for_source(src).add_hex_module("result", "pub fn is_ok() {}"),
        find_position_of("reult").select_until(find_position_of("."))
    );
}

#[test]
fn test_no_action_to_import_module_without_value() {
    // The language server should not suggest a code action
    // to import a module if it doesn't have a value with
    // the same name as we are trying to access
    let src = "
pub fn main() {
  io.hello_world()
}
";

    let title = "Import `io`";

    assert_no_code_actions!(
        title,
        TestProject::for_source(src).add_hex_module("io", "pub fn println() {}"),
        find_position_of("io").select_until(find_position_of("."))
    );
}

#[test]
fn test_no_action_to_import_module_without_type() {
    let src = "type Name = int.String";

    let title = "Import `int`";

    assert_no_code_actions!(
        title,
        TestProject::for_source(src).add_hex_module("int", ""),
        find_position_of("int").select_until(find_position_of("."))
    );
}

#[test]
fn test_no_action_to_import_module_with_private_value() {
    // The language server should not suggest a code action
    // to import a module if the value we are trying to
    // access is private.
    let src = "
pub fn main() {
  mod.internal()
}
";

    let title = "Import `mod`";

    assert_no_code_actions!(
        title,
        TestProject::for_source(src).add_hex_module("mod", "fn internal() {}"),
        find_position_of("mod").select_until(find_position_of("."))
    );
}

#[test]
fn test_no_action_to_import_module_with_private_type() {
    let src = "type T = module.T";

    let title = "Import `module`";

    assert_no_code_actions!(
        title,
        TestProject::for_source(src).add_hex_module("module", "type T { T }"),
        find_position_of("module").select_until(find_position_of("."))
    );
}

#[test]
fn test_no_action_to_import_module_with_constructor_named_same_as_type() {
    let src = "type NotAType = shapes.Rectangle";

    let title = "Import `shapes`";

    assert_no_code_actions!(
        title,
        TestProject::for_source(src)
            .add_hex_module("shapes", "pub type Shape { Rectangle, Circle }"),
        find_position_of("shapes").select_until(find_position_of("."))
    );
}

#[test]
fn add_missing_patterns_bool() {
    assert_code_action!(
        ADD_MISSING_PATTERNS,
        "
pub fn main(bool: Bool) {
  case bool {}
}
",
        find_position_of("case").select_until(find_position_of("bool {"))
    );
}

#[test]
fn add_missing_patterns_custom_type() {
    assert_code_action!(
        ADD_MISSING_PATTERNS,
        "
type Wibble {
  Wibble
  Wobble
  Wubble
}

pub fn main(wibble: Wibble) {
  case wibble {
    Wobble -> Nil
  }
}
",
        find_position_of("case").select_until(find_position_of("wibble {"))
    );
}

#[test]
fn add_missing_patterns_tuple() {
    assert_code_action!(
        ADD_MISSING_PATTERNS,
        "
pub fn main(two_at_once: #(Bool, Result(Int, Nil))) {
  case two_at_once {
    #(False, Error(_)) -> Nil
  }
}
",
        find_position_of("case").select_until(find_position_of("two_at_once {"))
    );
}

#[test]
fn add_missing_patterns_list() {
    assert_code_action!(
        ADD_MISSING_PATTERNS,
        "
pub fn main() {
  let list = [1, 2, 3]
  case list {
    [a, b, c, 4 as d] -> d
  }
}
",
        find_position_of("case").select_until(find_position_of("list {"))
    );
}

#[test]
fn add_missing_patterns_infinite() {
    assert_code_action!(
        ADD_MISSING_PATTERNS,
        r#"
pub fn main() {
  let value = 3
  case value {
    1 -> "one"
    2 -> "two"
    3 -> "three"
  }
}
"#,
        find_position_of("case").select_until(find_position_of("value {"))
    );
}

#[test]
fn add_missing_patterns_multi() {
    assert_code_action!(
        ADD_MISSING_PATTERNS,
        r#"
pub fn main(a: Bool) {
  let b = 1
  case a, b {

  }
}
"#,
        find_position_of("case").select_until(find_position_of("b {"))
    );
}

#[test]
fn add_missing_patterns_inline() {
    // Ensure we correctly detect the indentation, if the case expression
    // does not start at the beginning of the line
    assert_code_action!(
        ADD_MISSING_PATTERNS,
        r#"
pub fn main(a: Bool) {
  let value = case a {}
}
"#,
        find_position_of("case").select_until(find_position_of("a {"))
    );
}

#[test]
fn import_module_from_pattern() {
    let src = "
pub fn main(res) {
  case res {
    result.Ok(_) -> Nil
    result.Error(_) -> Nil
  }
}
";

    assert_code_action!(
        "Import `result`",
        TestProject::for_source(src)
            .add_hex_module("result", "pub type Result(v, e) { Ok(v) Error(e) }"),
        find_position_of("result").select_until(find_position_of("."))
    );
}

#[test]
fn annotate_function() {
    assert_code_action!(
        ADD_ANNOTATIONS,
        r#"
pub fn add_one(thing) {
  thing + 1
}
"#,
        find_position_of("fn").select_until(find_position_of("("))
    );
}

#[test]
fn annotate_function_with_annotated_return_type() {
    assert_code_action!(
        ADD_ANNOTATION,
        r#"
pub fn add_one(thing) -> Int {
  thing + 1
}
"#,
        find_position_of("fn").select_until(find_position_of("("))
    );
}

#[test]
fn annotate_function_with_partially_annotated_parameters() {
    assert_code_action!(
        ADD_ANNOTATION,
        r#"
pub fn add(a: Float, b) -> Float {
  a +. b
}
"#,
        find_position_of("fn").select_until(find_position_of("("))
    );
}

#[test]
fn no_code_action_for_fully_annotated_function() {
    assert_no_code_actions!(
        ADD_ANNOTATION | ADD_ANNOTATIONS,
        r#"
pub fn do_a_thing(a: Int, b: Float) -> String {
  todo
}
"#,
        find_position_of("fn").select_until(find_position_of("("))
    );
}

#[test]
fn annotate_anonymous_function() {
    assert_code_action!(
        ADD_ANNOTATIONS,
        r#"
pub fn add_curry(a) {
  fn(b) { a + b }
}
"#,
        find_position_of("fn(").select_until(find_position_of("b)"))
    );
}

#[test]
fn annotate_anonymous_function_with_annotated_return_type() {
    assert_code_action!(
        ADD_ANNOTATION,
        r#"
pub fn add_curry(a) {
  fn(b) -> Int { a + b }
}
"#,
        find_position_of("fn(").select_until(find_position_of("b)"))
    );
}

#[test]
fn annotate_anonymous_function_with_partially_annotated_parameters() {
    assert_code_action!(
        ADD_ANNOTATIONS,
        r#"
pub fn main() {
  fn(a, b: Int, c) { a + b + c }
}
"#,
        find_position_of("fn(").select_until(find_position_of("c)"))
    );
}

#[test]
fn no_code_action_for_fully_annotated_anonymous_function() {
    assert_no_code_actions!(
        ADD_ANNOTATION | ADD_ANNOTATIONS,
        r#"
pub fn main() {
  fn(a: Int, b: Int) -> Int { a - b }
}
"#,
        find_position_of("fn(").select_until(find_position_of("Int)"))
    );
}

#[test]
fn annotate_use() {
    assert_code_action!(
        ADD_ANNOTATIONS,
        r#"
pub fn wibble(wobble: fn(Int, Int) -> Int) {
  wobble(1, 2)
}

pub fn main() {
  use a, b <- wibble
  a + b
}
"#,
        find_position_of("use").select_until(find_position_of("<-"))
    );
}

#[test]
fn annotate_use_with_partially_annotated_parameters() {
    assert_code_action!(
        ADD_ANNOTATION,
        r#"
pub fn wibble(wobble: fn(Int, Int) -> Int) {
  wobble(1, 2)
}

pub fn main() {
  use a: Int, b <- wibble
  a + b
}
"#,
        find_position_of("use").select_until(find_position_of("<-"))
    );
}

#[test]
fn no_code_action_for_fully_annotated_use() {
    assert_no_code_actions!(
        ADD_ANNOTATION | ADD_ANNOTATIONS,
        r#"
pub fn wibble(wobble: fn(Int, Int) -> Int) {
  wobble(1, 2)
}

pub fn main() {
  use a: Int, b: Int <- wibble
  a + b
}
"#,
        find_position_of("use").select_until(find_position_of("<-"))
    );
}

#[test]
fn annotate_constant() {
    assert_code_action!(
        ADD_ANNOTATION,
        r#"
pub const my_constant = 20
"#,
        find_position_of("const").select_until(find_position_of("="))
    );
}

#[test]
fn no_code_action_for_annotated_constant() {
    assert_no_code_actions!(
        ADD_ANNOTATION | ADD_ANNOTATIONS,
        r#"
pub const PI: Float = 3.14159
"#,
        find_position_of("const").select_until(find_position_of("="))
    );
}

#[test]
fn annotate_local_variable() {
    assert_code_action!(
        ADD_ANNOTATION,
        r#"
pub fn main() {
  let my_value = 10
}
"#,
        find_position_of("let").select_until(find_position_of("="))
    );
}

#[test]
fn annotate_local_variable_with_pattern() {
    assert_code_action!(
        ADD_ANNOTATION,
        r#"
type Wibble {
  Wibble(a: Int, b: Int, c: Int)
}

pub fn main() {
  let Wibble(a, b, c) = Wibble(1, 2, 3)
}
"#,
        find_position_of("let").select_until(find_position_of("="))
    );
}

#[test]
fn annotate_local_variable_with_pattern2() {
    assert_code_action!(
        ADD_ANNOTATION,
        r#"
pub fn main(values) {
  let #(left, right) = values
}
"#,
        find_position_of("let").select_until(find_position_of("="))
    );
}

#[test]
fn annotate_local_variable_let_assert() {
    assert_code_action!(
        ADD_ANNOTATION,
        r#"
pub fn fallible() -> Result(Int, Nil) {
  todo
}

pub fn main() {
  let assert Ok(value) = fallible()
}
"#,
        find_position_of("let").select_until(find_position_of("="))
    );
}

#[test]
fn annotate_nested_local_variable() {
    assert_code_action!(
        ADD_ANNOTATION,
        r#"
pub fn main() {
  let a = {
    let b = 10
    b + 1
  }
}
"#,
        find_position_of("let b").select_until(find_position_of("b ="))
    );
}

#[test]
fn no_code_action_for_annotated_local_variable() {
    assert_no_code_actions!(
        ADD_ANNOTATION | ADD_ANNOTATIONS,
        r#"
pub fn main() {
  let typed: Int = 1.2
}
"#,
        find_position_of("let").select_until(find_position_of("="))
    );
}

#[test]
fn adding_annotations_correctly_prints_type_variables() {
    assert_code_action!(
        ADD_ANNOTATIONS,
        r#"
pub fn map_result(input, function) {
  case input {
    Ok(value) -> Ok(function(value))
    Error(error) -> Error(error)
  }
}
"#,
        find_position_of("fn").select_until(find_position_of("("))
    );
}

#[test]
fn add_multiple_annotations() {
    assert_code_action!(
        ADD_ANNOTATIONS,
        r#"
pub const my_constant = 20

pub fn add_my_constant(value) {
  let result = value + my_constant
  result
}
"#,
        find_position_of("pub const").select_until(find_position_of("}"))
    );
}

#[test]
fn different_annotations_create_compatible_type_variables() {
    assert_code_action!(
        ADD_ANNOTATIONS,
        r#"
pub fn do_generic_things(a, b) {
  let a_value = a
  let b_value = b
  let other_value = a_value
}
"#,
        find_position_of("let a_value").select_until(find_position_of("}"))
    );
}

#[test]
fn adding_annotations_prints_type_variable_names() {
    assert_code_action!(
        ADD_ANNOTATIONS,
        r#"
pub fn do_generic_things(a: type_a, b: type_b) {
  let a_value = a
  let b_value = b
  let other_value = a_value
}
"#,
        find_position_of("let a_value").select_until(find_position_of("}"))
    );
}

#[test]
fn adding_annotations_prints_contextual_types() {
    assert_code_action!(
        ADD_ANNOTATION,
        r#"
pub type IntAlias = Int

pub fn main() {
  let value = 20
}
"#,
        find_position_of("let").select_until(find_position_of("value"))
    );
}

#[test]
fn adding_annotations_prints_contextual_types2() {
    assert_code_action!(
        ADD_ANNOTATION,
        r#"
pub type Result

pub fn main() {
  let value = Ok(12)
}
"#,
        find_position_of("let").select_until(find_position_of("="))
    );
}

#[test]
fn adding_annotations_prints_contextual_types3() {
    let src = r#"
import wibble

pub fn main() {
  let value = wibble.Wibble
}
"#;

    assert_code_action!(
        ADD_ANNOTATION,
        TestProject::for_source(src).add_hex_module("wibble", "pub type Wibble { Wibble }"),
        find_position_of("let").select_until(find_position_of("="))
    );
}

#[test]
fn adding_annotations_prints_contextual_types4() {
    let src = r#"
import wibble as wobble

pub fn main() {
  let value = wobble.Wibble
}
"#;

    assert_code_action!(
        ADD_ANNOTATION,
        TestProject::for_source(src).add_hex_module("wibble", "pub type Wibble { Wibble }"),
        find_position_of("let").select_until(find_position_of("="))
    );
}

#[test]
fn adding_annotations_prints_contextual_types5() {
    let src = r#"
import wibble.{type Wibble as Wobble}

pub fn main() {
  let value = wibble.Wibble
}
"#;

    assert_code_action!(
        ADD_ANNOTATION,
        TestProject::for_source(src).add_hex_module("wibble", "pub type Wibble { Wibble }"),
        find_position_of("let").select_until(find_position_of("="))
    );
}

#[test]
// https://github.com/gleam-lang/gleam/issues/3789
fn no_code_actions_to_add_annotations_for_pipe() {
    assert_no_code_actions!(
        ADD_ANNOTATION | ADD_ANNOTATIONS,
        r#"
fn do_something(a: Int) { a }

pub fn main() {
  10 |> do_something
}
"#,
        find_position_of("10").select_until(find_position_of("|>"))
    );
}

#[test]
// https://github.com/gleam-lang/gleam/issues/3789#issuecomment-2455805734
fn add_correct_type_annotation_for_non_variable_use() {
    assert_code_action!(
        ADD_ANNOTATION,
        r#"
fn usable(f) {
  f(#(1, 2))
}

pub fn main() {
  use #(a, b) <- usable
  a + b
}
"#,
        find_position_of("use").select_until(find_position_of("b)"))
    );
}

#[test]
fn test_qualified_to_unqualified_import_basic_with_argument() {
    let src = r#"
import option

pub fn main() {
  option.Some(1)
}
"#;
    assert_code_action!(
        "Unqualify option.Some",
        TestProject::for_source(src)
            .add_hex_module("option", "pub type Option(v) { Some(v) None }"),
        find_position_of(".Some").select_until(find_position_of("(1)")),
    );
}

#[test]
fn test_qualified_to_unqualified_import_basic_record_without_argument() {
    let src = r#"
import wobble

pub fn main() {
  wobble.Wibble
}
"#;
    assert_code_action!(
        "Unqualify wobble.Wibble",
        TestProject::for_source(src).add_hex_module("wobble", "pub type Wobble { Wibble }"),
        find_position_of(".W").select_until(find_position_of("ibble"))
    );
}

#[test]
fn test_qualified_to_unqualified_import_basic_type_without_argument() {
    let src = r#"
import wobble

pub fn identity(x: wobble.Wobble) -> wobble.Wobble {
    x
}
"#;
    assert_code_action!(
        "Unqualify wobble.Wobble",
        TestProject::for_source(src).add_hex_module("wobble", "pub type Wobble { Wibble }"),
        find_position_of(".").select_until(find_position_of("Wobble"))
    );
}

#[test]
fn test_qualified_to_unqualified_import_basic_multiple() {
    let src = r#"
import option

pub fn main() {
  option.Some(1)
  option.Some(1)
  todo
}
"#;
    assert_code_action!(
        "Unqualify option.Some",
        TestProject::for_source(src)
            .add_hex_module("option", "pub type Option(v) { Some(v) None }"),
        find_position_of(".Some").select_until(find_position_of("(1)")),
    );
}

#[test]
fn test_qualified_to_unqualified_import_when_unqualified_exists() {
    let src = r#"
import option.{Some}

pub fn main() {
  option.Some(1)
}
"#;
    assert_code_action!(
        "Unqualify option.Some",
        TestProject::for_source(src)
            .add_hex_module("option", "pub type Option(v) { Some(v) None }"),
        find_position_of(".Some").select_until(find_position_of("(1)")),
    );
}

#[test]
fn test_qualified_to_unqualified_import_with_comma() {
    let src = r#"
import option.{None, }

pub fn main() {
  option.Some(1)
}
"#;
    assert_code_action!(
        "Unqualify option.Some",
        TestProject::for_source(src)
            .add_hex_module("option", "pub type Option(v) { Some(v) None }"),
        find_position_of(".Some").select_until(find_position_of("(1)")),
    );
}

#[test]
fn test_qualified_to_unqualified_import_with_comma_pos_not_end() {
    let src = r#"
import option.{None,   } as opt

pub fn main() {
  opt.Some(1)
}
"#;
    assert_code_action!(
        "Unqualify opt.Some",
        TestProject::for_source(src)
            .add_hex_module("option", "pub type Option(v) { Some(v) None }"),
        find_position_of(".Some").select_until(find_position_of("(1)")),
    );
}

#[test]
fn test_qualified_to_unqualified_import_different_constructors() {
    let src = r#"
import option

pub fn main() {
  option.Some(1)
  option.None
}"#;
    assert_code_action!(
        "Unqualify option.Some",
        TestProject::for_source(src)
            .add_hex_module("option", "pub type Option(v) { Some(v) None }"),
        find_position_of(".Some").select_until(find_position_of("(1)")),
    );
}

#[test]
fn test_qualified_to_unqualified_import_no_action_when_already_unqualified() {
    let src = r#"
import option.{Some, None}

pub fn main() {
  Some(1)
  Some(1)
  todo
}
"#;
    let title = "Unqualify option.Some";
    assert_no_code_actions!(
        title,
        TestProject::for_source(src)
            .add_hex_module("option", "pub type Option(v) { Some(v) None }"),
        find_position_of("Some(").select_until(find_position_of("1)")),
    );
}

#[test]
fn test_qualified_to_unqualified_import_with_alias() {
    let src = r#"
import option as opt

pub fn main() {
  opt.Some(1)
}
"#;
    assert_code_action!(
        "Unqualify opt.Some",
        TestProject::for_source(src)
            .add_hex_module("option", "pub type Option(v) { Some(v) None }"),
        find_position_of("opt.Some").select_until(find_position_of("(1)")),
    );
}

#[test]
fn test_qualified_to_unqualified_import_with_alias_multiple() {
    let src = r#"
import option as opt

pub fn main() {
  opt.Some(1)
  opt.Some(1)
}

pub fn identity(x: opt.Option(Int)) -> opt.Option(Int) {
    opt.Some(1)
    x
}
"#;
    assert_code_action!(
        "Unqualify opt.Some",
        TestProject::for_source(src)
            .add_hex_module("option", "pub type Option(v) { Some(v) None }"),
        find_position_of("opt.Some").select_until(find_position_of("(1)")),
    );
}

#[test]
fn test_qualified_to_unqualified_import_multiple_imports() {
    let src = r#"
import option
import wobble

pub fn main() {
  option.Some(2)
  wobble.Wibble(1)
}
"#;
    assert_code_action!(
        "Unqualify wobble.Wibble",
        TestProject::for_source(src)
            .add_hex_module("option", "pub type Option(v) { Some(v) None }")
            .add_hex_module("wobble", "pub type Wobble { Wibble(Int)} "),
        find_position_of(".Wibble").select_until(find_position_of("(1)")),
    );
}

#[test]
fn test_qualified_to_unqualified_import_in_case_with_argument() {
    let src = r#"
import option

pub fn main(x) {
  case option.Some(1) {
    option.Some(value) -> value
    option.None -> 0
  }
}
"#;
    assert_code_action!(
        "Unqualify option.Some",
        TestProject::for_source(src)
            .add_hex_module("option", "pub type Option(v) { Some(v) None }"),
        find_position_of(".Some(").select_until(find_position_of("(1)"))
    );
}

#[test]
fn test_qualified_to_unqualified_import_in_case_without_argument() {
    let src = r#"
import wobble

pub fn main() {
  case wobble.Wibble {
    wobble.Wibble -> 1
    wobble.Wubble(1) -> 2
  }
}
"#;
    assert_code_action!(
        "Unqualify wobble.Wibble",
        TestProject::for_source(src)
            .add_hex_module("wobble", "pub type Wobble { Wibble Wubble(Int) }"),
        find_position_of(".W").select_until(find_position_of("ibble"))
    );
}

#[test]
fn test_qualified_to_unqualified_import_in_pattern() {
    let src = r#"
import option

pub fn main() -> Int {
  case option.Some(1) {
    option.Some(value) -> value
    option.None -> 0
  }
}
"#;
    assert_code_action!(
        "Unqualify option.Some",
        TestProject::for_source(src)
            .add_hex_module("option", "pub type Option(v) { Some(v) None }"),
        find_position_of(".Some(va").select_until(find_position_of("lue)")),
    );
}

#[test]
fn test_qualified_to_unqualified_import_in_pattern_without_argument() {
    let src = r#"
import wobble

pub fn main() {
  case wobble.Wibble {
    wobble.Wibble -> 1
    wobble.Wubble(1) -> 2
  }
  let wob = wobble.Wibble
  todo
}
"#;
    assert_code_action!(
        "Unqualify wobble.Wibble",
        TestProject::for_source(src)
            .add_hex_module("wobble", "pub type Wobble { Wibble Wubble(Int) }"),
        find_position_of("wobble.W").select_until(find_position_of("ibble"))
    );
}

#[test]
fn test_qualified_to_unqualified_import_type() {
    let src = r#"
import option

pub fn main(x) -> option.Option(Int) {
    option.Some(1)
}
"#;
    assert_code_action!(
        "Unqualify option.Option",
        TestProject::for_source(src)
            .add_hex_module("option", "pub type Option(v) { Some(v) None }"),
        find_position_of(".Option").select_until(find_position_of("(Int)")),
    );
}

#[test]
fn test_qualified_to_unqualified_import_nested_type_outer() {
    let src = r#"
import option
import wobble
pub fn main(x) -> option.Option(wobble.Wibble) {
    todo
}
"#;
    assert_code_action!(
        "Unqualify option.Option",
        TestProject::for_source(src)
            .add_hex_module("option", "pub type Option(v) { Some(v) None }")
            .add_hex_module("wobble", "pub type Wibble { Wobble(Int) }"),
        find_position_of(".O").select_until(find_position_of("ption(")),
    );
}

#[test]
fn test_qualified_to_unqualified_import_nested_constructor_outer() {
    let src = r#"
import option
import wobble
pub fn main(x) -> option.Option(wobble.Wibble) {
    option.Some(wobble.Wobble(1))
}
"#;
    assert_code_action!(
        "Unqualify option.Some",
        TestProject::for_source(src)
            .add_hex_module("option", "pub type Option(v) { Some(v) None }")
            .add_hex_module("wobble", "pub type Wibble { Wobble(Int) }"),
        find_position_of(".S").select_until(find_position_of("ome(")),
    );
}

#[test]
fn test_qualified_to_unqualified_import_nested_constructor_inner() {
    let src = r#"
import option
import wobble

pub fn main(x) -> option.Option(wobble.Wibble) {
    option.Some(wobble.Wobble(1))
}
"#;
    assert_code_action!(
        "Unqualify wobble.Wobble",
        TestProject::for_source(src)
            .add_hex_module("option", "pub type Option(v) { Some(v) None }")
            .add_hex_module("wobble", "pub type Wibble { Wobble(Int) }"),
        find_position_of(".Wobble").select_until(find_position_of("(1)")),
    );
}

#[test]
fn test_qualified_to_unqualified_import_nested_type_inner() {
    let src = r#"
import option
import wobble

pub fn main(x) -> option.Option(wobble.Wibble) {
    todo
}
"#;
    assert_code_action!(
        "Unqualify wobble.Wibble",
        TestProject::for_source(src)
            .add_hex_module("option", "pub type Option(v) { Some(v) None }")
            .add_hex_module("wobble", "pub type Wibble { Wobble(Int) }"),
        find_position_of("wobble.").select_until(find_position_of("Wibble")),
    );
}

#[test]
fn test_qualified_to_unqualified_import_below_constructor() {
    let src = r#"

pub fn main() {
  option.Some(1)
}

import option
"#;
    assert_code_action!(
        "Unqualify option.Some",
        TestProject::for_source(src)
            .add_hex_module("option", "pub type Option(v) { Some(v) None }"),
        find_position_of(".Some").select_until(find_position_of("(1)")),
    );
}

#[test]
fn test_qualified_to_unqualified_import_between_constructors() {
    let src = r#"

pub fn main() {
  option.Some(1)
}

import option

pub fn identity(x: option.Option(Int)) -> option.Option(Int) {
    option.Some(1)
    x
}
"#;
    assert_code_action!(
        "Unqualify option.Some",
        TestProject::for_source(src)
            .add_hex_module("option", "pub type Option(v) { Some(v) None }"),
        find_position_of(".Some").select_until(find_position_of("(1)")),
    );
}

#[test]
fn test_qualified_to_unqualified_import_multiple_line() {
    let src = r#"
import option.{
    type Option,
    None,
}

pub fn main() {
  option.Some(1)
}
"#;
    assert_code_action!(
        "Unqualify option.Some",
        TestProject::for_source(src)
            .add_hex_module("option", "pub type Option(v) { Some(v) None }"),
        find_position_of(".Some").select_until(find_position_of("(1)")),
    );
}

#[test]
fn test_qualified_to_unqualified_import_multiple_line_bad_format_with_trailing_comma() {
    let src = r#"
import option.{type Option,
    None,

}

pub fn main() {
  option.Some(1)
}
"#;
    assert_code_action!(
        "Unqualify option.Some",
        TestProject::for_source(src)
            .add_hex_module("option", "pub type Option(v) { Some(v) None }"),
        find_position_of(".Some").select_until(find_position_of("(1)")),
    );
}

#[test]
fn test_qualified_to_unqualified_import_multiple_line_bad_format_multiple_whitespace() {
    let src = r#"
import option.{    }

pub fn main() {
  option.Some(1)
}
"#;
    assert_code_action!(
        "Unqualify option.Some",
        TestProject::for_source(src)
            .add_hex_module("option", "pub type Option(v) { Some(v) None }"),
        find_position_of(".Some").select_until(find_position_of("(1)")),
    );
}
#[test]
fn test_qualified_to_unqualified_import_multiple_line_bad_format_without_trailing_comma() {
    let src = r#"
import option.{type Option,
    None

}

pub fn main() {
  option.Some(1)
}
"#;
    assert_code_action!(
        "Unqualify option.Some",
        TestProject::for_source(src)
            .add_hex_module("option", "pub type Option(v) { Some(v) None }"),
        find_position_of(".Some").select_until(find_position_of("(1)")),
    );
}
#[test]
fn test_qualified_to_unqualified_import_multiple_line_aliased() {
    let src = r#"
import option.{
    type Option,
    None} as opt

pub fn main() {
  opt.Some(1)
}
"#;
    assert_code_action!(
        "Unqualify opt.Some",
        TestProject::for_source(src)
            .add_hex_module("option", "pub type Option(v) { Some(v) None }"),
        find_position_of(".Some").select_until(find_position_of("(1)")),
    );
}

#[test]
fn test_qualified_to_unqualified_import_in_list_and_tuple() {
    let src = r#"
import option

pub fn main() {
    let list = [option.Some(1), option.None]
    let tuple = #(option.Some(2), option.None)
}
"#;
    assert_code_action!(
        "Unqualify option.Some",
        TestProject::for_source(src)
            .add_hex_module("option", "pub type Option(v) { Some(v) None }"),
        find_position_of("option.Some").select_until(find_position_of("(1)")),
    );
}

#[test]
fn test_qualified_to_unqualified_import_multiple_generic_type() {
    let src = r#"
import result

pub fn main() -> result.Result(Int, String) {
    result.Ok(1)
}
"#;
    assert_code_action!(
        "Unqualify result.Result",
        TestProject::for_source(src)
            .add_hex_module("result", "pub type Result(a, e) { Ok(a) Error(e) }"),
        find_position_of(".Result").select_until(find_position_of("(Int")),
    );
}

#[test]
fn test_qualified_to_unqualified_import_constructor_as_argument() {
    let src = r#"
import option

pub fn main() {
    option.map(option.Some(1), fn(x) { x + 1 })
}
"#;
    assert_code_action!(
        "Unqualify option.Some",
        TestProject::for_source(src).add_hex_module(
            "option",
            "
            pub type Option(v) { Some(v) None }
            pub fn map(a, f) { todo }
            "
        ),
        find_position_of("option.Some").select_until(find_position_of("(1)")),
    );
}

#[test]
fn test_qualified_to_unqualified_import_constructor_different_module_same_type_inner() {
    let src = r#"
import option
import opt

pub fn main() -> option.Option(opt.Option(Int)) {
    todo
}
"#;
    assert_code_action!(
        "Unqualify opt.Option",
        TestProject::for_source(src)
            .add_hex_module("option", "pub type Option(v) { Some(v) None }")
            .add_hex_module("opt", "pub type Option(v) { Some(v) None }"),
        find_position_of("opt.Option").select_until(find_position_of("(Int)")),
    );
}
#[test]
fn test_qualified_to_unqualified_import_constructor_different_module_same_type_outer() {
    let src = r#"
import option
import opt

pub fn main() -> option.Option(opt.Option(Int)) {
    todo
}
"#;
    assert_code_action!(
        "Unqualify option.Option",
        TestProject::for_source(src)
            .add_hex_module("option", "pub type Option(v) { Some(v) None }")
            .add_hex_module("opt", "pub type Option(v) { Some(v) None }"),
        find_position_of("option.").select_until(find_position_of("Option(")),
    );
}
#[test]
fn test_qualified_to_unqualified_import_constructor_different_module_same_name_inner() {
    let src = r#"
import option
import opt

pub fn main() {
    option.Some(opt.Some(1))
    todo
}
"#;
    assert_code_action!(
        "Unqualify opt.Some",
        TestProject::for_source(src)
            .add_hex_module("option", "pub type Option(v) { Some(v) None }")
            .add_hex_module("opt", "pub type Option(v) { Some(v) None }"),
        find_position_of("opt.Some").select_until(find_position_of("(1)")),
    );
}
#[test]
fn test_qualified_to_unqualified_import_constructor_different_module_same_name_outer() {
    let src = r#"
import option
import opt

pub fn main() {
    option.Some(opt.Some(1))
}
"#;
    assert_code_action!(
        "Unqualify option.Some",
        TestProject::for_source(src)
            .add_hex_module("option", "pub type Option(v) { Some(v) None }")
            .add_hex_module("opt", "pub type Option(v) { Some(v) None }"),
        find_position_of("option.").select_until(find_position_of("Some(")),
    );
}

#[test]
fn test_qualified_to_unqualified_import_constructor_complex_pattern() {
    let src = r#"
import option

pub fn main() {
    case [option.Some(1), option.None] {
        [option.None, ..] -> todo
        [option.Some(_), ..] -> todo
        _ -> todo
    }
    case option.Some(1), option.Some(2) {
        option.None, option.Some(_) -> todo
        option.Some(_), option.Some(val) -> todo
        _ -> todo
    }
}
"#;
    assert_code_action!(
        "Unqualify option.Some",
        TestProject::for_source(src)
            .add_hex_module("option", "pub type Option(v) { Some(v) None }"),
        find_position_of("option.").select_until(find_position_of("Some(")),
    );
}

#[test]
fn test_qualified_to_unqualified_import_constructor_constructor_name_exists() {
    let src = r#"
import option.{Some}
import opt

pub fn main() -> option.Option(opt.Option(Int)) {
    Some(opt.Some(1))
}
"#;
    let title = "Unqualify opt.Some";
    assert_no_code_actions!(
        title,
        TestProject::for_source(src)
            .add_hex_module("option", "pub type Option(v) { Some(v) None }")
            .add_hex_module("opt", "pub type Option(v) { Some(v) None }"),
        find_position_of("opt.").select_until(find_position_of(".Some(")),
    );
}

#[test]
fn test_qualified_to_unqualified_import_constructor_constructor_name_exists_below() {
    let src = r#"
import opt

pub fn main() -> option.Option(opt.Option(Int)) {
    Some(opt.Some(1))
}
import option.{Some}
"#;
    let title = "Unqualify opt.Some";
    assert_no_code_actions!(
        title,
        TestProject::for_source(src)
            .add_hex_module("option", "pub type Option(v) { Some(v) None }")
            .add_hex_module("opt", "pub type Option(v) { Some(v) None }"),
        find_position_of("opt.").select_until(find_position_of(".Some(")),
    );
}

#[test]
fn test_qualified_to_unqualified_import_type_constructor_constructor_name_exists() {
    let src = r#"
import option.{type Option}
import opt

pub fn main() -> Option(opt.Option(Int)) {
    option.Some(opt.Some(1))
}
"#;
    let title = "Unqualify opt.Option";
    assert_no_code_actions!(
        title,
        TestProject::for_source(src)
            .add_hex_module("option", "pub type Option(v) { Some(v) None }")
            .add_hex_module("opt", "pub type Option(v) { Some(v) None }"),
        find_position_of("opt.").select_until(find_position_of(".Option(")),
    );
}

#[test]
fn test_qualified_to_unqualified_import_type_constructor_constructor_name_exists_below() {
    let src = r#"
import opt

pub fn main() -> Option(opt.Option(Int)) {
    option.Some(opt.Some(1))
}
import option.{type Option}
"#;
    let title = "Unqualify opt.Option";
    assert_no_code_actions!(
        title,
        TestProject::for_source(src)
            .add_hex_module("option", "pub type Option(v) { Some(v) None }")
            .add_hex_module("opt", "pub type Option(v) { Some(v) None }"),
        find_position_of("opt.").select_until(find_position_of(".Option(")),
    );
}
#[test]
fn test_unqualified_to_qualified_import_function() {
    let src = r#"
import list.{map}

pub fn main() {
    let identity = map([1, 2, 3], fn(x) { x })
    let double = map([1, 2, 3], fn(x) { x * 2 })
}
"#;
    assert_code_action!(
        "Qualify map as list.map",
        TestProject::for_source(src).add_hex_module("list", "pub fn map(list, f) { todo }"),
        find_position_of("map(").select_until(find_position_of("[1, 2, 3]")),
    );
}

#[test]
fn test_unqualified_to_qualified_import_constant() {
    let src = r#"
import mymath.{pi}

pub fn circle_area(radius: Float) -> Float {
    pi *. radius *. radius
}

pub fn circle_circumference(radius: Float) -> Float {
    2. *. pi *. radius
}
"#;
    assert_code_action!(
        "Qualify pi as mymath.pi",
        TestProject::for_source(src).add_hex_module("mymath", "pub const pi = 3.14159"),
        find_position_of("pi *.").select_until(find_position_of(" radius")),
    );
}

#[test]
fn test_unqualified_to_qualified_import_record_constructor() {
    let src = r#"
import user.{type User, User}

pub fn create_user(name: String) -> User {
    User(name: name, id: 1)
}
"#;
    assert_code_action!(
        "Qualify User as user.User",
        TestProject::for_source(src)
            .add_hex_module("user", "pub type User { User(name: String, id: Int) }"),
        find_position_of("User(").select_until(find_position_of("name: name")),
    );
}

#[test]
fn test_unqualified_to_qualified_import_after_constructor() {
    let src = r#"
pub fn create_user(name: String) -> User {
    User(name: name, id: 1)
}

import user.{type User, User}
"#;
    assert_code_action!(
        "Qualify User as user.User",
        TestProject::for_source(src)
            .add_hex_module("user", "pub type User { User(name: String, id: Int) }"),
        find_position_of("User(").select_until(find_position_of("name: name")),
    );
}

#[test]
fn test_unqualified_to_qualified_import_between_constructors() {
    let src = r#"
pub fn create_user(name: String) -> User {
    User(name: name, id: 1)
}

import user.{type User, User}

pub fn user_list(users: List(User)) -> List(String) {
    [User(name: "John", id: 1),
    User(name: "Jane", id: 2)]
}

"#;
    assert_code_action!(
        "Qualify User as user.User",
        TestProject::for_source(src)
            .add_hex_module("user", "pub type User { User(name: String, id: Int) }"),
        find_position_of("User(").select_until(find_position_of("name: name")),
    );
}

#[test]
fn test_unqualified_to_qualified_import_multiple_occurrences() {
    let src = r#"
import list.{map, filter}

pub fn process_list(items: List(Int)) -> List(Int) {
    items
    |> map(fn(x) { x + 1 })
    |> map(fn(x) { x * 2 })
}
"#;
    assert_code_action!(
        "Qualify map as list.map",
        TestProject::for_source(src).add_hex_module(
            "list",
            "pub fn map(list: List(a), with fun: fn(a) -> b) -> List(b) { todo }"
        ),
        find_position_of("|> map").select_until(find_position_of("(fn(x)")),
    );
}

#[test]
fn test_unqualified_to_qualified_import_in_pattern_matching() {
    let src = r#"
import result.{type Result, Ok, Error}

pub fn process_result(res: Result(Int, String)) -> Int {
    case res {
        Ok(value) -> value
        Error(_) -> 0
    }
}
"#;
    assert_code_action!(
        "Qualify Ok as result.Ok",
        TestProject::for_source(src)
            .add_hex_module("result", "pub type Result(a, e) { Ok(a) Error(e) }"),
        find_position_of("Ok(").select_until(find_position_of("value)")),
    );
}

#[test]
fn test_unqualified_to_qualified_import_type_annotation() {
    let src = r#"
import option.{type Option, Some}

pub fn maybe_increment(x: Option(Int)) -> Option(Int) {
    case x {
        Some(value) -> Some(value + 1)
        _ -> x
    }
}
"#;
    assert_code_action!(
        "Qualify Option as option.Option",
        TestProject::for_source(src)
            .add_hex_module("option", "pub type Option(a) { Some(a) None }"),
        find_position_of("Opt")
            .nth_occurrence(2)
            .select_until(find_position_of("ion(")),
    );
}

#[test]
fn test_unqualified_to_qualified_import_nested_function_call() {
    let src = r#"
import list.{map, flatten}
import operation.{double}

pub fn process_names(names: List(List(Int))) -> List(Int) {
    names
    |> flatten
    |> map(double)
}
"#;
    assert_code_action!(
        "Qualify double as operation.double",
        TestProject::for_source(src)
            .add_hex_module(
                "list",
                "pub fn map(list: List(a), with fun: fn(a) -> b) -> List(b) { todo }
pub fn flatten(lists: List(List(a))) -> List(a) { todo }"
            )
            .add_hex_module("operation", "pub fn double(s: Int) -> Int { todo }"),
        find_position_of("(dou").select_until(find_position_of("ble)")),
    );
}

#[test]
fn test_unqualified_to_qualified_import_with_alias() {
    let src = r#"
import list.{map as transform}

pub fn double_list(items: List(Int)) -> List(Int) {
    transform(items, fn(x) { x * 2 })
}
"#;
    assert_code_action!(
        "Qualify transform as list.map",
        TestProject::for_source(src).add_hex_module(
            "list",
            "pub fn map(list: List(a), with fun: fn(a) -> b) -> List(b) { todo }"
        ),
        find_position_of("transform(").select_until(find_position_of("items,")),
    );
}

#[test]
fn test_unqualified_to_qualified_import_with_alias_and_module_alias() {
    let src = r#"
import list.{map as transform} as lst

pub fn double_list(items: List(Int)) -> List(Int) {
    transform(items, fn(x) { x * 2 })
}
"#;
    assert_code_action!(
        "Qualify transform as lst.map",
        TestProject::for_source(src).add_hex_module(
            "list",
            "pub fn map(list: List(a), with fun: fn(a) -> b) -> List(b) { todo }"
        ),
        find_position_of("transform(").select_until(find_position_of("items,")),
    );
}

#[test]
fn test_unqualified_to_qualified_import_import_discarded() {
    let src = r#"
import list.{map as transform} as _

pub fn double_list(items: List(Int)) -> List(Int) {
    transform(items, fn(x) { x * 2 })
}
"#;
    let title = "Qualify transform as list.map";
    assert_no_code_actions!(
        title,
        TestProject::for_source(src).add_hex_module(
            "list",
            "pub fn map(list: List(a), with fun: fn(a) -> b) -> List(b) { todo }"
        ),
        find_position_of("transform(").select_until(find_position_of("items,")),
    );
}

#[test]
fn test_unqualified_to_qualified_import_bad_formatted_type_constructor() {
    let src = r#"
import option.{type    Option, Some}

pub fn maybe_increment(x: Option(Int)) -> Option(Int) {
    case x {
        Some(value) -> Some(value + 1)
        _ -> x
    }
}
"#;
    assert_code_action!(
        "Qualify Option as option.Option",
        TestProject::for_source(src)
            .add_hex_module("option", "pub type Option(a) { Some(a) None }"),
        find_position_of("Opt")
            .nth_occurrence(2)
            .select_until(find_position_of("ion(")),
    );
}

#[test]
fn test_unqualified_to_qualified_import_bad_formatted_type_constructor_with_alias() {
    let src = r#"
import option.{type    Option    as Maybe, Some}

pub fn maybe_increment(x: Maybe(Int)) -> Maybe(Int) {
    case x {
        Some(value) -> Some(value + 1)
        _ -> x
    }
}
"#;
    assert_code_action!(
        "Qualify Maybe as option.Option",
        TestProject::for_source(src)
            .add_hex_module("option", "pub type Option(a) { Some(a) None }"),
        find_position_of("May")
            .nth_occurrence(2)
            .select_until(find_position_of("be(")),
    );
}

#[test]
fn test_unqualified_to_qualified_import_bad_formatted_comma() {
    let src = r#"
import option.{type    Option    , Some}

pub fn maybe_increment(x: Option(Int)) -> Option(Int) {
    case x {
        Some(value) -> Some(value + 1)
        _ -> x
    }
}
"#;
    assert_code_action!(
        "Qualify Option as option.Option",
        TestProject::for_source(src)
            .add_hex_module("option", "pub type Option(a) { Some(a) None }"),
        find_position_of("Opt")
            .nth_occurrence(2)
            .select_until(find_position_of("ion(")),
    );
}

#[test]
fn test_unqualified_to_qualified_import_in_list_and_tuple() {
    let src = r#"
import option.{Some}

pub fn main() {
    let list = [Some(1), option.None]
    let tuple = #(Some(2), option.None)
}
"#;
    assert_code_action!(
        "Qualify Some as option.Some",
        TestProject::for_source(src)
            .add_hex_module("option", "pub type Option(v) { Some(v) None }"),
        find_position_of("Some(").select_until(find_position_of("1)")),
    );
}
#[test]
fn test_unqualified_to_qualified_import_constructor_complex_pattern() {
    let src = r#"
import option.{None, Some}

pub fn main() {
    case [Some(1), None] {
        [None, ..] -> todo
        [Some(_), ..] -> todo
        _ -> todo
    }
    case Some(1), Some(2) {
        None, Some(_) -> todo
        Some(_), Some(val) -> todo
        _ -> todo
    }
}
"#;
    assert_code_action!(
        "Qualify Some as option.Some",
        TestProject::for_source(src)
            .add_hex_module("option", "pub type Option(v) { Some(v) None }"),
        find_position_of("Some(").select_until(find_position_of("1)")),
    );
}

#[test]
fn test_unqualified_to_qualified_import_multiple_line_aliased() {
    let src = r#"
import option.{
    type Option,
    None,
    Some
} as opt

pub fn main() {
  Some(1)
}
"#;
    assert_code_action!(
        "Qualify Some as opt.Some",
        TestProject::for_source(src)
            .add_hex_module("option", "pub type Option(v) { Some(v) None }"),
        find_position_of("Some")
            .nth_occurrence(2)
            .select_until(find_position_of("(1)")),
    );
}

#[test]
fn test_unqualified_to_qualified_import_multiple_line_bad_format_without_trailing_comma() {
    let src = r#"
import option.{type Option,
    Some

}

pub fn main() {
  Some(1)
}
"#;
    assert_code_action!(
        "Qualify Some as option.Some",
        TestProject::for_source(src)
            .add_hex_module("option", "pub type Option(v) { Some(v) None }"),
        find_position_of("Some")
            .nth_occurrence(2)
            .select_until(find_position_of("(1)")),
    );
}
/* TODO: implement qualified unused location
#[test]
fn test_remove_unused_qualified_action() {
    let code = "
// test
import map.{Map, delete}
";
    let expected = "
// test

";
    assert_eq!(remove_unused_action(code), expected.to_string())
}

#[test]
fn test_remove_unused_qualified_partial_action() {
    let code = "
// test
import result.{is_ok, is_err}

pub fn main() {
  is_ok
}
";
    let expected = "
// test
import result.{is_ok}

pub fn main() {
  is_ok
}
";
    assert_eq!(remove_unused_action(code), expected.to_string())
}

#[test]
fn test_remove_unused_qualified_partial2_action() {
    let code = "
// test
import result.{all, is_ok, is_err}

pub fn main() {
  is_ok
}
";
    let expected = "
// test
import result.{ is_ok}

pub fn main() {
  is_ok
}
";
    assert_eq!(remove_unused_action(code), expected.to_string())
}

#[test]
fn test_remove_unused_qualified_partial3_action() {
    let code = "
// test
import result.{all, is_ok, is_err} as res

pub fn main() {
  is_ok
}
";
    let expected = "
// test
import result.{ is_ok} as res

pub fn main() {
  is_ok
}
";
    assert_eq!(remove_unused_action(code), expected.to_string())
}
*/

#[test]
fn desugar_use_expression_with_no_parens() {
    let src = r#"
pub fn main() {
  use <- wibble
  todo
  todo
}

fn wibble(f) {
    f()
}
"#;
    assert_code_action!(
        DESUGAR_USE_EXPRESSION,
        TestProject::for_source(src),
        find_position_of("use").select_until(find_position_of("todo")),
    );
}

#[test]
fn desugar_use_expression_with_empty_parens() {
    let src = r#"
pub fn main() {
  use <- wibble()
  todo
  todo
}

fn wibble(f) {
    f()
}
"#;
    assert_code_action!(
        DESUGAR_USE_EXPRESSION,
        TestProject::for_source(src),
        find_position_of("use").to_selection(),
    );
}

#[test]
fn desugar_use_expression_with_parens_and_other_args() {
    let src = r#"
pub fn main() {
  use <- wibble(1, 2)
  todo
  todo
}

fn wibble(n, m, f) {
    f()
}
"#;
    assert_code_action!(
        DESUGAR_USE_EXPRESSION,
        TestProject::for_source(src),
        find_position_of("wibble").select_until(find_position_of("1")),
    );
}

#[test]
fn desugar_use_expression_with_single_pattern() {
    let src = r#"
pub fn main() {
  use a <- wibble(1, 2)
  todo
  todo
}

fn wibble(n, m, f) {
    f(1)
}
"#;
    assert_code_action!(
        DESUGAR_USE_EXPRESSION,
        TestProject::for_source(src),
        find_position_of("todo").to_selection(),
    );
}

#[test]
fn desugar_use_expression_with_multiple_patterns() {
    let src = r#"
pub fn main() {
  use a, b <- wibble(1, 2)
  todo
  todo
}

fn wibble(n, m, f) {
    f(1, 2)
}
"#;
    assert_code_action!(
        DESUGAR_USE_EXPRESSION,
        TestProject::for_source(src),
        find_position_of("todo").nth_occurrence(2).to_selection(),
    );
}

#[test]
fn desugar_nested_use_expressions_picks_inner_under_cursor() {
    let src = r#"
pub fn main() {
  use a, b <- wibble(1, 2)
  use a, b <- wibble(a, b)
  todo
}

fn wibble(n, m, f) {
    f(1, 2)
}
"#;
    assert_code_action!(
        DESUGAR_USE_EXPRESSION,
        TestProject::for_source(src),
        find_position_of("todo").under_last_char().to_selection(),
    );
}

#[test]
fn desugar_nested_use_expressions_picks_inner_under_cursor_2() {
    let src = r#"
pub fn main() {
  use a, b <- wibble(1, 2)
  use a, b <- wibble(a, b)
  todo
}

fn wibble(n, m, f) {
    f(1, 2)
}
"#;
    assert_code_action!(
        DESUGAR_USE_EXPRESSION,
        TestProject::for_source(src),
        find_position_of("<-").select_until(find_position_of("wibble")),
    );
}

#[test]
fn desugar_use_expression_with_type_annotations() {
    let src = r#"
pub fn main() {
  use a: Int, b: Int <- wibble(1, 2)
  todo
}

fn wibble(n, m, f) {
    f(1, 2)
}
"#;
    assert_code_action!(
        DESUGAR_USE_EXPRESSION,
        TestProject::for_source(src),
        find_position_of("<-").select_until(find_position_of("wibble")),
    );
}

#[test]
fn desugar_use_expression_doesnt_work_with_complex_patterns() {
    let src = r#"
pub fn main() {
  use #(a, b), 1 <- wibble(1, 2)
  todo
}

fn wibble(n, m, f) {
    f(todo, todo)
}
"#;
    assert_no_code_actions!(
        DESUGAR_USE_EXPRESSION,
        TestProject::for_source(src),
        find_position_of("<-").select_until(find_position_of("wibble")),
    );
}

#[test]
fn turn_call_into_use_with_single_line_body() {
    let src = r#"
pub fn main() {
  wibble(fn(a, b) { todo })
}

fn wibble(f) {
  f(todo, todo)
}
"#;
    assert_code_action!(
        CONVERT_TO_USE,
        TestProject::for_source(src),
        find_position_of("wibble").to_selection(),
    );
}

#[test]
fn turn_call_into_use_with_fn_with_no_args() {
    let src = r#"
pub fn main() {
  wibble(fn() { todo })
}

fn wibble(f) {
  f()
}
"#;
    assert_code_action!(
        CONVERT_TO_USE,
        TestProject::for_source(src),
        find_position_of("wibble").to_selection(),
    );
}

#[test]
fn turn_call_with_multiple_arguments_into_use() {
    let src = r#"
pub fn main() {
  wibble(1, 2, fn(a) { todo })
}

fn wibble(m, n, f) {
  f(1)
}
"#;
    assert_code_action!(
        CONVERT_TO_USE,
        TestProject::for_source(src),
        find_position_of("todo").to_selection(),
    );
}

#[test]
fn turn_call_with_multiline_fn_into_use() {
    let src = r#"
pub fn main() {
  wibble(1, 2, fn(a) {
    todo
    case todo {
      _ -> todo
    }
  })
}

fn wibble(m, n, f) {
  f(1)
}
"#;
    assert_code_action!(
        CONVERT_TO_USE,
        TestProject::for_source(src),
        find_position_of("1, 2").select_until(find_position_of("fn(a)")),
    );
}

#[test]
fn turn_call_with_fn_with_type_annotations_into_use() {
    let src = r#"
pub fn main() {
  wibble(1, 2, fn(a: Int) {
    todo
  })
}

fn wibble(m, n, f) {
  f(1)
}
"#;
    assert_code_action!(
        CONVERT_TO_USE,
        TestProject::for_source(src),
        find_position_of("wibble").select_until(find_position_of("a: ")),
    );
}

#[test]
fn turn_call_into_use_only_works_on_last_call_in_a_block() {
    let src = r#"
pub fn main() {
  wibble(10, 20, fn(a) { todo })
  wibble(1, 2, fn(a) { todo })
}

fn wibble(m, n, f) {
  f(1)
}
"#;
    assert_no_code_actions!(
        CONVERT_TO_USE,
        TestProject::for_source(src),
        find_position_of("10").to_selection(),
    );
}

#[test]
fn turn_call_into_use_only_works_on_last_call_in_a_block_2() {
    let src = r#"
pub fn main() {
  {
    wibble(10, 20, fn(a) { todo })
    wibble(1, 2, fn(a) { todo })
  }
  Nil
}

fn wibble(m, n, f) {
  f(1)
}
"#;
    assert_no_code_actions!(
        CONVERT_TO_USE,
        TestProject::for_source(src),
        find_position_of("10").to_selection(),
    );
}

#[test]
fn turn_call_into_use_with_last_function_in_a_block() {
    let src = r#"
pub fn main() {
  {
    wibble(10, 20, fn(a) { todo })
    wibble(1, 11, fn(a) { todo })
  }
  Nil
}

fn wibble(m, n, f) {
  f(1)
}
"#;
    assert_code_action!(
        CONVERT_TO_USE,
        TestProject::for_source(src),
        find_position_of("wibble(1,").select_until(find_position_of("11")),
    );
}

#[test]
fn turn_call_into_use_starts_from_innermost_function() {
    let src = r#"
pub fn main() {
  wibble(10, 20, fn(a) {
    wibble(30, 40, fn(b) {
      a + b
    })
  })
}

fn wibble(m, n, f) {
  f(1)
}
"#;
    assert_code_action!(
        CONVERT_TO_USE,
        TestProject::for_source(src),
        find_position_of("30").select_until(find_position_of("40")),
    );
}

#[test]
fn turn_call_into_use_with_another_use_in_the_way() {
    let src = r#"
pub fn main() {
  wibble(10, 20, fn(a) {
    use b <- wibble(30, 40)
    a + b
  })
}

fn wibble(m, n, f) {
  f(1)
}
"#;
    assert_code_action!(
        CONVERT_TO_USE,
        TestProject::for_source(src),
        find_position_of("use").to_selection(),
    );
}

#[test]
fn turn_call_into_use_with_module_function() {
    let src = r#"
import other
pub fn main() {
  other.wibble(10, 20, fn(a) {
    todo
    a + b
  })
}
"#;
    assert_code_action!(
        CONVERT_TO_USE,
        TestProject::for_source(src).add_module("other", "pub fn wibble(n, m, f) { todo }"),
        find_position_of("wibble").to_selection(),
    );
}

#[test]
fn inexhaustive_let_result_to_case() {
    assert_code_action!(
        CONVERT_TO_CASE,
        "pub fn main(result) {
  let Ok(value) = result
}",
        find_position_of("let").select_until(find_position_of("=")),
    );
}

#[test]
fn inexhaustive_let_to_case_indented() {
    assert_code_action!(
        CONVERT_TO_CASE,
        "pub fn main(result) {
  {
    let Ok(value) = result
  }
}",
        find_position_of("let").select_until(find_position_of("=")),
    );
}

#[test]
fn inexhaustive_let_to_case_multi_variables() {
    assert_code_action!(
        CONVERT_TO_CASE,
        "pub fn main() {
  let [var1, var2, _var3, var4] = [1, 2, 3, 4]
}",
        find_position_of("let").select_until(find_position_of("=")),
    );
}

#[test]
fn inexhaustive_let_to_case_discard() {
    assert_code_action!(
        CONVERT_TO_CASE,
        "pub fn main() {
  let [_elem] = [6]
}",
        find_position_of("let").select_until(find_position_of("=")),
    );
}

#[test]
fn inexhaustive_let_to_case_no_variables() {
    assert_code_action!(
        CONVERT_TO_CASE,
        "pub fn main() {
  let [] = []
}",
        find_position_of("let").select_until(find_position_of("=")),
    );
}

#[test]
fn inexhaustive_let_alias_to_case() {
    assert_code_action!(
        CONVERT_TO_CASE,
        "pub fn main() {
  let 10 as ten = 10
}",
        find_position_of("let").select_until(find_position_of("=")),
    );
}

#[test]
fn inexhaustive_let_tuple_to_case() {
    assert_code_action!(
        CONVERT_TO_CASE,
        "pub fn main() {
  let #(first, 10, third) = #(5, 10, 15)
}
",
        find_position_of("let").select_until(find_position_of("=")),
    );
}

#[test]
fn inexhaustive_let_bit_array_to_case() {
    assert_code_action!(
        CONVERT_TO_CASE,
        "pub fn main() {
  let <<bits1, bits2>> = <<73, 98>>
}",
        find_position_of("let").select_until(find_position_of("=")),
    );
}

#[test]
fn inexhaustive_let_string_prefix_to_case() {
    assert_code_action!(
        CONVERT_TO_CASE,
        r#"pub fn main() {
  let "_" <> thing = "_Hello"
}"#,
        find_position_of("let").select_until(find_position_of("=")),
    );
}

#[test]
fn inexhaustive_let_string_prefix_pattern_alias_to_case() {
    assert_code_action!(
        CONVERT_TO_CASE,
        r#"pub fn main() {
  let "123" as one_two_three <> rest = "123456"
}"#,
        find_position_of("let").select_until(find_position_of("=")),
    );
}

#[test]
fn inner_inexhaustive_let_to_case() {
    assert_code_action!(
        CONVERT_TO_CASE,
        r#"pub fn main(result) {
  let [wibble] = {
    let Ok(wobble) = {
      result
    }
    [wobble]
  }
}"#,
        find_position_of("let Ok").select_until(find_position_of(") =")),
    );
}

#[test]
fn outer_inexhaustive_let_to_case() {
    assert_code_action!(
        CONVERT_TO_CASE,
        r#"pub fn main(result) {
  let [wibble] = {
    let Ok(wobble) = {
      result
    }
    [wobble]
  }
}"#,
        find_position_of("let [").select_until(find_position_of("] =")),
    );
}

#[test]
fn no_code_action_for_exhaustive_let_to_case() {
    assert_no_code_actions!(
        CONVERT_TO_CASE,
        r#"pub fn first(pair) {
  let #(first, second) = pair
  first
}"#,
        find_position_of("let").select_until(find_position_of("=")),
    );
}

#[test]
fn extract_variable() {
    assert_code_action!(
        EXTRACT_VARIABLE,
        r#"pub fn main() {
  list.map([1, 2, 3], int.add(1, _))
}"#,
        find_position_of("[1").select_until(find_position_of("2"))
    );
}

#[test]
fn extract_variable_does_not_extract_a_variable() {
    assert_no_code_actions!(
        EXTRACT_VARIABLE,
        r#"pub fn main() {
    let z = 1
    let a = [1, 2, z]
}"#,
        find_position_of("z").nth_occurrence(2).to_selection()
    );
}

#[test]
fn extract_variable_2() {
    assert_code_action!(
        EXTRACT_VARIABLE,
        r#"pub fn main() {
  list.map([1, 2, 3], int.add(1, _))
}"#,
        find_position_of("int.").select_until(find_position_of("add"))
    );
}

#[test]
fn extract_variable_from_capture_arguments() {
    assert_no_code_actions!(
        EXTRACT_VARIABLE,
        r#"pub fn main() {
  int.add(1, _)
}"#,
        find_position_of("_").to_selection()
    );
}

#[test]
fn extract_variable_from_capture_arguments_2() {
    assert_code_action!(
        EXTRACT_VARIABLE,
        r#"pub fn main() {
  int.add(11, _)
}"#,
        find_position_of("11").to_selection()
    );
}

#[test]
fn extract_variable_3() {
    assert_code_action!(
        EXTRACT_VARIABLE,
        r#"pub fn main() {
  list.map([1, 2, 3], todo, todo)
}"#,
        find_position_of("todo")
            .nth_occurrence(2)
            .select_until(find_position_of("todo)").under_last_char())
    );
}

#[test]
fn extract_variable_inside_multiline_function_call() {
    assert_code_action!(
        EXTRACT_VARIABLE,
        r#"pub fn main() {
  list.map(
    [1, 2, 3],
    int.add(1, _),
  )
}"#,
        find_position_of("[1").to_selection()
    );
}

#[test]
fn extract_variable_in_case_branch() {
    assert_code_action!(
        EXTRACT_VARIABLE,
        r#"pub fn main() {
    case wibble {
      _ -> [1, 2, 3]
    }
}"#,
        find_position_of("[1").to_selection()
    );
}

#[test]
fn extract_variable_in_multiline_case_subject_branch() {
    assert_code_action!(
        EXTRACT_VARIABLE,
        r#"pub fn main() {
    case
        list.map(
          [1, 2, 3],
          int.add(1, _)
        )
    {
      _ -> todo
    }
}"#,
        find_position_of("[1").to_selection()
    );
}

#[test]
fn extract_variable_in_use() {
    assert_code_action!(
        EXTRACT_VARIABLE,
        r#"pub fn main() {
    use <- wibble([1, 2, 3])
    todo
}"#,
        find_position_of("[1").to_selection()
    );
}

#[test]
fn extract_variable_inside_use_body() {
    assert_code_action!(
        EXTRACT_VARIABLE,
        r#"pub fn main() {
    use <- wibble(todo)
    list.map([1, 2, 3], int.add(1, _))
    todo
}"#,
        find_position_of("[1").to_selection()
    );
}

#[test]
fn extract_variable_in_multiline_use() {
    assert_code_action!(
        EXTRACT_VARIABLE,
        r#"pub fn main() {
    use <- wibble(
        [1, 2, 3]
    )
    todo
}"#,
        find_position_of("[1").to_selection()
    );
}

#[test]
fn extract_variable_in_block() {
    assert_code_action!(
        EXTRACT_VARIABLE,
        r#"pub fn main() {
  {
    todo
    wibble([1, 2, 3])
    todo
  }
}"#,
        find_position_of("2").select_until(find_position_of("3"))
    );
}
