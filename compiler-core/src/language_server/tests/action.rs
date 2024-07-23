use crate::line_numbers::LineNumbers;
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

/// This function replicates how the text editor applies TextEdit.
///
fn apply_code_edit(src: &str, changes: HashMap<Url, Vec<lsp_types::TextEdit>>) -> String {
    let mut result = src.to_string();
    let line_numbers = LineNumbers::new(src);
    let mut offset = 0;
    for (_, mut change) in changes {
        change.sort_by_key(|edit| (edit.range.start.line, edit.range.start.character));
        for edit in change {
            let start = line_numbers.byte_index(edit.range.start.line, edit.range.start.character)
                as i32
                - offset;
            let end = line_numbers.byte_index(edit.range.end.line, edit.range.end.character) as i32
                - offset;
            let range = (start as usize)..(end as usize);
            offset += end - start;
            offset -= edit.new_text.len() as i32;
            result.replace_range(range, &edit.new_text);
        }
    }
    result
}

const REMOVE_UNUSED_IMPORTS: &str = "Remove unused imports";
const REMOVE_REDUNDANT_TUPLES: &str = "Remove redundant tuples";
const CONVERT_TO_CASE: &str = "Convert to case";
const USE_LABEL_SHORTHAND_SYNTAX: &str = "Use label shorthand syntax";
const FILL_LABELS: &str = "Fill labels";
const ASSIGN_UNUSED_RESULT: &str = "Assign unused Result value to `_`";

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
    );}

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
