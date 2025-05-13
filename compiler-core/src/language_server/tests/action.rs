use itertools::Itertools;
use lsp_types::{
    CodeActionContext, CodeActionParams, PartialResultParams, Position, Range, Url,
    WorkDoneProgressParams,
};

use super::*;

fn code_actions(tester: &TestProject<'_>, range: Range) -> Option<Vec<lsp_types::CodeAction>> {
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
    tester: &TestProject<'_>,
    range: Range,
) -> Vec<lsp_types::CodeAction> {
    code_actions(tester, range)
        .into_iter()
        .flatten()
        .filter(|action| titles.contains(&action.title.as_str()))
        .collect_vec()
}

fn owned_actions_with_title(
    titles: Vec<&str>,
    tester: TestProject<'_>,
    range: Range,
) -> Vec<lsp_types::CodeAction> {
    actions_with_title(titles, &tester, range)
}

fn apply_code_action(title: &str, tester: TestProject<'_>, range: Range) -> String {
    let titles = vec![title];
    let changes = actions_with_title(titles, &tester, range)
        .pop()
        .expect("No action with the given title")
        .edit
        .expect("No workspace edit found")
        .changes
        .expect("No text edit found");
    apply_code_edit(tester, changes)
}

fn apply_code_edit(
    tester: TestProject<'_>,
    changes: HashMap<Url, Vec<lsp_types::TextEdit>>,
) -> String {
    let mut changed_files: HashMap<Url, String> = HashMap::new();
    for (uri, change) in changes {
        let code = match changed_files.get(&uri) {
            Some(code) => code,
            None => tester
                .src_from_module_url(&uri)
                .expect(&format!("no src for url {:?}", uri)),
        };
        let code = super::apply_code_edit(code, change);
        let _ = changed_files.insert(uri, code);
    }

    show_code_edits(tester, changed_files)
}

fn show_code_edits(tester: TestProject<'_>, changed_files: HashMap<Url, String>) -> String {
    let format_code = |url: &Url, code: &String| {
        format!(
            "// --- Edits applied to module '{}'\n{}",
            tester.module_name_from_url(url).expect("a module"),
            code
        )
    };

    // If the file that changed is the main one we just show its code.
    if changed_files.len() == 1 {
        let mut changed = changed_files.iter().peekable();
        let (url, code) = changed.peek().unwrap();
        if tester.module_name_from_url(url) == Some("app".into()) {
            code.to_string()
        } else {
            format_code(url, code)
        }
    } else {
        // If more than a single file changed we want to add the name of the
        // file before each!
        changed_files
            .iter()
            .map(|(url, code)| format_code(url, code))
            .join("\n")
    }
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
const CONVERT_FROM_USE: &str = "Convert from `use`";
const CONVERT_TO_USE: &str = "Convert to `use`";
const EXTRACT_VARIABLE: &str = "Extract variable";
const EXTRACT_CONSTANT: &str = "Extract constant";
const EXPAND_FUNCTION_CAPTURE: &str = "Expand function capture";
const GENERATE_DYNAMIC_DECODER: &str = "Generate dynamic decoder";
const GENERATE_TO_JSON_FUNCTION: &str = "Generate to-JSON function";
const PATTERN_MATCH_ON_ARGUMENT: &str = "Pattern match on argument";
const PATTERN_MATCH_ON_VARIABLE: &str = "Pattern match on variable";
const GENERATE_FUNCTION: &str = "Generate function";
const CONVERT_TO_FUNCTION_CALL: &str = "Convert to function call";
const INLINE_VARIABLE: &str = "Inline variable";
const CONVERT_TO_PIPE: &str = "Convert to pipe";
const INTERPOLATE_STRING: &str = "Interpolate string";
const FILL_UNUSED_FIELDS: &str = "Fill unused fields";
const REMOVE_ALL_ECHOS_FROM_THIS_MODULE: &str = "Remove all `echo`s from this module";
const WRAP_IN_BLOCK: &str = "Wrap in block";
const GENERATE_VARIANT: &str = "Generate variant";

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
        let result = owned_actions_with_title(all_titles, $project, range);
        assert_eq!(expected, result);
    };
}

#[test]
fn fix_truncated_segment_1() {
    let name = "Replace with `1`";
    assert_code_action!(
        name,
        r#"
pub fn main() {
  <<1, 257, 259:size(1)>>
}"#,
        find_position_of("257").to_selection()
    );
}

#[test]
fn fix_truncated_segment_2() {
    let name = "Replace with `0`";
    assert_code_action!(
        name,
        r#"
pub fn main() {
  <<1, 1024:size(10)>>
}"#,
        find_position_of("size").to_selection()
    );
}

#[test]
fn generate_variant_with_fields_in_same_module() {
    assert_code_action!(
        GENERATE_VARIANT,
        r#"
pub type Wibble {
  Wibble
}

pub fn main() -> Wibble {
  Wobble(1)
}"#,
        find_position_of("Wobble").to_selection()
    );
}

#[test]
fn generate_variant_with_no_fields_in_same_module() {
    assert_code_action!(
        GENERATE_VARIANT,
        r#"
pub type Wibble {
  Wibble
}

pub fn main() -> Wibble {
  Wobble
}"#,
        find_position_of("Wobble").to_selection()
    );
}

#[test]
fn generate_variant_with_labels_in_same_module() {
    assert_code_action!(
        GENERATE_VARIANT,
        r#"
pub type Wibble {
  Wibble
}

pub fn main() -> Wibble {
  Wobble("hello", label: 1)
}"#,
        find_position_of("Wobble").to_selection()
    );
}

#[test]
fn generate_variant_from_pattern_with_fields() {
    assert_code_action!(
        GENERATE_VARIANT,
        r#"
pub type Wibble {
  Wibble
}

pub fn new() { Wibble }

pub fn main() -> Wibble {
  let assert Wobble(1) = new()
}

"#,
        find_position_of("Wobble").to_selection()
    );
}

#[test]
fn generate_variant_from_pattern_with_labelled_fields() {
    assert_code_action!(
        GENERATE_VARIANT,
        r#"
pub type Wibble {
  Wibble
}

pub fn new() { Wibble }

pub fn main() -> Wibble {
  let assert Wobble("hello", label: 1) = new()
}

"#,
        find_position_of("Wobble").to_selection()
    );
}

#[test]
fn generate_variant_from_pattern_with_no_fields() {
    assert_code_action!(
        GENERATE_VARIANT,
        r#"
pub type Wibble {
  Wibble
}

pub fn new() { Wibble }

pub fn main() -> Wibble {
  let assert Wobble = new()
}

"#,
        find_position_of("Wobble").to_selection()
    );
}

#[test]
fn generate_unqualified_variant_in_other_module() {
    let src = r#"
import other

pub fn main() -> other.Wibble {
  let assert Wobble = new()
}

pub fn new() -> other.Wibble { todo }
"#;

    assert_code_action!(
        GENERATE_VARIANT,
        TestProject::for_source(src).add_module("other", "pub type Wibble"),
        find_position_of("Wobble").to_selection()
    );
}

#[test]
fn generate_qualified_variant_in_other_module() {
    let src = r#"
import other

pub fn main() -> other.Wibble {
  let assert other.Wobble = new()
}

pub fn new() -> other.Wibble { todo }
"#;
    assert_code_action!(
        GENERATE_VARIANT,
        TestProject::for_source(src).add_module("other", "pub type Wibble"),
        find_position_of("Wobble").to_selection()
    );
}

#[test]
fn do_not_generate_variant_if_one_with_the_same_name_exists() {
    assert_no_code_actions!(
        GENERATE_VARIANT,
        r#"
pub fn main() -> Wibble {
  let assert Wobble = new()
}

pub type Wibble {
  Wobble(n: Int)
}

pub fn new() -> Wibble { todo }
"#,
        find_position_of("Wobble").to_selection()
    );
}

#[test]
fn do_not_generate_variant_if_one_with_the_same_name_exists_in_other_module() {
    let src = r#"
import other.{type Wibble}

pub fn main() -> Wibble {
  let assert Wobble = new()
}

pub fn new() -> Wibble { todo }
"#;
    assert_no_code_actions!(
        GENERATE_VARIANT,
        TestProject::for_source(src).add_module("other", "pub type Wibble { Wobble(String) }"),
        find_position_of("Wobble").to_selection()
    );
}

#[test]
fn do_not_generate_qualified_variant_if_one_with_the_same_name_exists_in_other_module() {
    let src = r#"
import other.{type Wibble}

pub fn main() -> Wibble {
  let assert other.Wobble = new()
}

pub fn new() -> Wibble { todo }
"#;
    assert_no_code_actions!(
        GENERATE_VARIANT,
        TestProject::for_source(src).add_module("other", "pub type Wibble { Wobble(String) }"),
        find_position_of("Wobble").to_selection()
    );
}

#[test]
fn fill_unused_fields_with_ignored_labelled_fields() {
    assert_code_action!(
        FILL_UNUSED_FIELDS,
        r#"
pub type Wibble { Wibble(Int, label1: String, label2: Int) }

pub fn main() {
  let Wibble(_, ..) = todo
}"#,
        find_position_of("..").to_selection()
    );
}

#[test]
fn fill_unused_fields_with_ignored_positional_fields() {
    assert_code_action!(
        FILL_UNUSED_FIELDS,
        r#"
pub type Wibble { Wibble(Int, label1: String, label2: Int) }

pub fn main() {
  let Wibble(label1:, label2:, ..) = todo
}"#,
        find_position_of("..").to_selection()
    );
}

#[test]
fn fill_unused_fields_with_all_positional_fields() {
    assert_code_action!(
        FILL_UNUSED_FIELDS,
        r#"
pub type Wibble { Wibble(Int, String) }

pub fn main() {
  let Wibble(..) = todo
}"#,
        find_position_of("..").to_selection()
    );
}

#[test]
fn fill_unused_fields_with_ignored_mixed_fields() {
    assert_code_action!(
        FILL_UNUSED_FIELDS,
        r#"
pub type Wibble { Wibble(Int, String, label1: String, label2: Int) }

pub fn main() {
  let Wibble(_, label2:, ..) = todo
}"#,
        find_position_of("..").to_selection()
    );
}

#[test]
fn fill_unused_fields_with_all_ignored_fields() {
    assert_code_action!(
        FILL_UNUSED_FIELDS,
        r#"
pub type Wibble { Wibble(Int, label1: String, label2: Int) }

pub fn main() {
  let Wibble(..) = todo
}"#,
        find_position_of("..").to_selection()
    );
}

#[test]
fn fill_unused_fields_with_ignored_fields_never_calls_a_positional_arg_as_a_labelled_one() {
    assert_code_action!(
        FILL_UNUSED_FIELDS,
        r#"
pub type Wibble { Wibble(Int, int: Int) }

pub fn main() {
  let Wibble(..) = todo
}"#,
        find_position_of("..").to_selection()
    );
}

#[test]
fn remove_echo() {
    assert_code_action!(
        REMOVE_ALL_ECHOS_FROM_THIS_MODULE,
        "pub fn main() {
  echo 1 + 2
}",
        find_position_of("echo").to_selection()
    );
}

#[test]
fn remove_echo_selecting_expression() {
    assert_code_action!(
        REMOVE_ALL_ECHOS_FROM_THIS_MODULE,
        "pub fn main() {
  echo 1 + 2
}",
        find_position_of("1").select_until(find_position_of("2"))
    );
}

#[test]
fn remove_echo_as_function_arg() {
    assert_code_action!(
        REMOVE_ALL_ECHOS_FROM_THIS_MODULE,
        "pub fn main() {
  wibble([], echo 1 + 2)
}",
        find_position_of("1").to_selection()
    );
}

#[test]
fn remove_echo_in_pipeline_step() {
    assert_code_action!(
        REMOVE_ALL_ECHOS_FROM_THIS_MODULE,
        "pub fn main() {
  [1, 2, 3]
  |> echo
  |> wibble
}",
        find_position_of("echo").to_selection()
    );
}

#[test]
fn remove_echo_in_single_line_pipeline_step() {
    assert_code_action!(
        REMOVE_ALL_ECHOS_FROM_THIS_MODULE,
        "pub fn main() {
  [1, 2, 3] |> echo |> wibble
}",
        find_position_of("echo").to_selection()
    );
}

#[test]
fn remove_echo_last_in_long_pipeline_step() {
    assert_code_action!(
        REMOVE_ALL_ECHOS_FROM_THIS_MODULE,
        "pub fn main() {
  [1, 2, 3]
  |> wibble
  |> echo
}",
        find_position_of("echo").to_selection()
    );
}

#[test]
fn remove_echo_last_in_short_pipeline_step() {
    assert_code_action!(
        REMOVE_ALL_ECHOS_FROM_THIS_MODULE,
        "pub fn main() {
  [1, 2, 3]
  |> echo
}",
        find_position_of("echo").to_selection()
    );
}

#[test]
fn remove_echo_before_pipeline() {
    assert_code_action!(
        REMOVE_ALL_ECHOS_FROM_THIS_MODULE,
        "pub fn main() {
  echo [1, 2, 3] |> wibble
}",
        find_position_of("echo").to_selection()
    );
}

#[test]
fn remove_echo_before_pipeline_selecting_step() {
    assert_code_action!(
        REMOVE_ALL_ECHOS_FROM_THIS_MODULE,
        "pub fn main() {
  echo [1, 2, 3] |> wibble
}",
        find_position_of("wibble").to_selection()
    );
}

#[test]
fn remove_echo_removes_all_echos() {
    assert_code_action!(
        REMOVE_ALL_ECHOS_FROM_THIS_MODULE,
        "pub fn main() {
  echo wibble(echo 1, 2)
}",
        find_position_of("echo").nth_occurrence(2).to_selection()
    );
}

#[test]
fn remove_echo_removes_all_echos_1() {
    assert_code_action!(
        REMOVE_ALL_ECHOS_FROM_THIS_MODULE,
        "pub fn main() {
  echo 1 |> echo |> echo |> wibble |> echo
  echo wibble(echo 1, echo 2)
  echo 1
}",
        find_position_of("echo").nth_occurrence(2).to_selection()
    );
}

#[test]
fn remove_echo_removes_entire_echo_statement_used_with_literals() {
    assert_code_action!(
        REMOVE_ALL_ECHOS_FROM_THIS_MODULE,
        "pub fn main() {
  echo 1
  Nil
}",
        find_position_of("echo").to_selection()
    );
}

#[test]
fn remove_echo_removes_entire_echo_statement_used_with_a_var() {
    assert_code_action!(
        REMOVE_ALL_ECHOS_FROM_THIS_MODULE,
        "pub fn main() {
  let a = 1
  echo a
  Nil
}",
        find_position_of("echo").to_selection()
    );
}

#[test]
fn remove_echo_removes_multiple_entire_echo_statement_used_with_literals() {
    assert_code_action!(
        REMOVE_ALL_ECHOS_FROM_THIS_MODULE,
        r#"pub fn main() {
  echo 1
  echo "wibble"
  Nil
}"#,
        find_position_of("echo").to_selection()
    );
}

#[test]
fn remove_echo_removes_multiple_entire_echo_statement_used_with_literals_but_stops_at_comments() {
    assert_code_action!(
        REMOVE_ALL_ECHOS_FROM_THIS_MODULE,
        r#"pub fn main() {
  echo 1

  // Oh no I hope I'm not deleted by the code action!!
  Nil
}"#,
        find_position_of("echo").to_selection()
    );
}

#[test]
fn remove_echo_removes_entire_echo_statement_used_with_literals_in_a_fn() {
    assert_code_action!(
        REMOVE_ALL_ECHOS_FROM_THIS_MODULE,
        "pub fn main() {
  fn() {
    echo 1
    Nil
  }
}",
        find_position_of("echo").to_selection()
    );
}

#[test]
fn remove_echo_removes_multiple_entire_echo_statement_used_with_literals_in_a_fn() {
    assert_code_action!(
        REMOVE_ALL_ECHOS_FROM_THIS_MODULE,
        r#"pub fn main() {
  fn() {
    echo 1
    echo "wibble"
    Nil
  }
}"#,
        find_position_of("echo").to_selection()
    );
}

#[test]
fn remove_echo_removes_does_not_remove_entire_echo_statement_if_its_the_return() {
    assert_code_action!(
        REMOVE_ALL_ECHOS_FROM_THIS_MODULE,
        "pub fn main() {
  echo 1
}",
        find_position_of("echo").to_selection()
    );
}

#[test]
fn remove_echo_removes_does_not_remove_entire_echo_statement_if_its_the_return_of_a_fn() {
    assert_code_action!(
        REMOVE_ALL_ECHOS_FROM_THIS_MODULE,
        r#"pub fn main() {
  fn() {
    echo 1
  }
}"#,
        find_position_of("echo").to_selection()
    );
}

#[test]
fn split_string() {
    assert_code_action!(
        INTERPOLATE_STRING,
        r#"pub fn main() {
  "wibble wobble woo"
}"#,
        find_position_of("wobble").to_selection()
    );
}

#[test]
fn no_split_string_right_at_the_start() {
    assert_no_code_actions!(
        INTERPOLATE_STRING,
        r#"pub fn main() {
  "wibble wobble woo"
}"#,
        find_position_of("wibble").to_selection()
    );
}

#[test]
fn no_split_string_right_at_the_end() {
    assert_no_code_actions!(
        INTERPOLATE_STRING,
        r#"pub fn main() {
  "wibble wobble woo"
}"#,
        find_position_of("\"").nth_occurrence(2).to_selection()
    );
}

#[test]
fn no_split_string_before_the_start() {
    assert_no_code_actions!(
        INTERPOLATE_STRING,
        r#"pub fn main() {
  "wibble wobble woo"
}"#,
        find_position_of("\"").to_selection()
    );
}

#[test]
fn no_split_string_after_the_end() {
    assert_no_code_actions!(
        INTERPOLATE_STRING,
        r#"pub fn main() {
  "wibble wobble woo"//we need this comment so we can put the cursor _after_ the closing quote
}"#,
        find_position_of("\"/").under_last_char().to_selection()
    );
}

#[test]
fn interpolate_string_inside_string() {
    assert_code_action!(
        INTERPOLATE_STRING,
        r#"pub fn main() {
  "wibble wobble woo"
}"#,
        find_position_of("wobble").select_until(find_position_of("wobble ").under_last_char()),
    );
}

#[test]
fn fallback_to_split_string_when_selecting_invalid_name() {
    assert_code_action!(
        INTERPOLATE_STRING,
        r#"pub fn main() {
  "wibble wobble woo woo"
}"#,
        find_position_of("wobble").select_until(find_position_of("woo ").under_last_char()),
    );
}

#[test]
fn splitting_string_as_first_pipeline_step_inserts_brackets() {
    assert_code_action!(
        INTERPOLATE_STRING,
        r#"pub fn main() {
  "wibble  wobble" |> io.println
}"#,
        find_position_of(" wobble").to_selection(),
    );
}

#[test]
fn interpolating_string_as_first_pipeline_step_inserts_brackets() {
    assert_code_action!(
        INTERPOLATE_STRING,
        r#"pub fn main() {
  "wibble wobble woo" |> io.println
}"#,
        find_position_of("wobble ").select_until(find_position_of("wobble ").under_last_char()),
    );
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
fn test_remove_unused_value() {
    let src = "
// test
import result.{is_ok}
import option

pub fn main() {
  result.is_ok
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
fn test_remove_aliased_unused_value() {
    let src = "
// test
import result.{is_ok as ok}
import option

pub fn main() {
  result.is_ok
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
fn test_remove_multiple_unused_values() {
    let src = "
// test
import result.{type Unused, used, unused, unused_again, type Used, used_again}

pub fn main(x: Used) {
  #(used, used_again)
}
";
    assert_code_action!(
        REMOVE_UNUSED_IMPORTS,
        TestProject::for_source(src).add_hex_module(
            "result",
            "
pub const used = 1
pub const unused = 2
pub const unused_again = 3
pub const used_again = 4
pub type Unused
pub type Used
"
        ),
        find_position_of("// test").select_until(find_position_of("pub")),
    );
}

#[test]
fn test_remove_multiple_unused_values_2() {
    let src = "
// test
import result.{type Unused, used, unused, type Used, unused_again}

pub fn main(x: Used) {
  used
}
";
    assert_code_action!(
        REMOVE_UNUSED_IMPORTS,
        TestProject::for_source(src).add_hex_module(
            "result",
            "
pub const used = 1
pub const unused = 2
pub const unused_again = 3
pub type Unused
pub type Used
"
        ),
        find_position_of("// test").select_until(find_position_of("pub")),
    );
}

#[test]
fn test_remove_entire_unused_import() {
    let src = "
// test
import result.{unused, unused_again}

pub fn main() {
  todo
}
";
    assert_code_action!(
        REMOVE_UNUSED_IMPORTS,
        TestProject::for_source(src).add_hex_module(
            "result",
            "
pub const used = 1
pub const unused = 2
pub const unused_again = 3
pub type Unused
pub type Used
"
        ),
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
fn fill_in_labelled_args_with_some_arguments_already_supplied() {
    assert_code_action!(
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
fn fill_in_labelled_args_with_some_arguments_already_supplied_2() {
    assert_code_action!(
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
fn fill_in_labelled_args_with_some_arguments_already_supplied_3() {
    assert_code_action!(
        FILL_LABELS,
        r#"
pub fn main() {
  wibble(1, arg3: 2)
}

pub fn wibble(arg1 arg1, arg2 arg2, arg3 arg3) { Nil }
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
  todo
}

pub fn wibble(arg1 arg1, arg2 arg2) { Nil }
 "#,
        find_position_of("wibble(").select_until(find_position_of("wibble()").under_last_char()),
    );
}

#[test]
fn fill_in_labelled_args_works_with_use_2() {
    assert_code_action!(
        FILL_LABELS,
        r#"
pub fn main() {
  use <- wibble(arg1: 1)
  todo
}

pub fn wibble(arg1 arg1, arg2 arg2, arg3 arg3) { Nil }
 "#,
        find_position_of("wibble(").select_until(find_position_of("1").under_last_char()),
    );
}

#[test]
fn fill_in_labelled_args_works_with_use_3() {
    assert_code_action!(
        FILL_LABELS,
        r#"
pub fn main() {
  use <- wibble(arg2: 2)
  todo
}

pub fn wibble(arg1 arg1, arg2 arg2, arg3 arg3) { Nil }
 "#,
        find_position_of("wibble(").select_until(find_position_of("2").under_last_char()),
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
    let x = 1
    Ok(x)
    Nil
}
"#,
        find_position_of("Ok").select_until(find_position_of("(x)")),
    );
}

#[test]
fn test_assign_unused_result_in_block() {
    assert_code_action!(
        ASSIGN_UNUSED_RESULT,
        r#"
pub fn main() {
    {
        let x = 1
        Ok(x)
        Nil
    }
    Nil
}
"#,
        find_position_of("Ok").select_until(find_position_of("(x)")),
    );
}

#[test]
fn test_assign_unused_result_on_block_start() {
    assert_code_action!(
        ASSIGN_UNUSED_RESULT,
        r#"
pub fn main() {
    {
        let x = 1
        Ok(x)
        Ok(x)
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
        let x = 1
        Ok(x)
        Ok(x)
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
        let x = 1
        Nil
        Ok(x)
    }
}
"#,
        find_position_of("Ok").select_until(find_position_of("(x)"))
    );
}

#[test]
fn test_assign_unused_result_only_first_action() {
    assert_code_action!(
        ASSIGN_UNUSED_RESULT,
        r#"
pub fn main() {
    let x = 1
    Ok(x)
    Ok(x)
    Nil
}
"#,
        find_position_of("Ok").select_until(find_position_of("(x)"))
    );
}

#[test]
#[should_panic(expected = "No action with the given title")]
fn test_assign_unused_result_not_on_return_value() {
    assert_code_action!(
        ASSIGN_UNUSED_RESULT,
        r#"
pub fn main() {
    let x = 1
    Ok(x)
}
"#,
        find_position_of("Ok").select_until(find_position_of("(x)"))
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
        let x = 1
        Ok(x)
    }
    Nil
}"#,
        find_position_of("Ok").select_until(find_position_of("(x)"))
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
fn test_qualified_to_unqualified_import_custom_type_record_declaration() {
    let src = r#"
import wobble

pub type Wibble {
  Wibble(wibble: wobble.Wobble)
}
"#;
    assert_code_action!(
        "Unqualify wobble.Wobble",
        TestProject::for_source(src).add_hex_module("wobble", "pub type Wobble { Wibble }"),
        find_position_of(".").select_until(find_position_of("Wobble"))
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
fn test_qualified_to_unqualified_record_value_constructor_module_name() {
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
        find_position_of("option").nth_occurrence(2).to_selection()
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

#[test]
fn test_unqualified_to_qualified_import_variable_shadowing() {
    let src = r#"

import wibble.{wobble}

pub fn example() {
  echo wobble

  let wobble = 1

  echo wobble

  let _ = fn(wobble) {
    echo wobble
  }

  todo
}
"#;

    assert_code_action!(
        "Qualify wobble as wibble.wobble",
        TestProject::for_source(src).add_hex_module("wibble", "pub fn wobble() { todo }"),
        find_position_of("wob")
            .nth_occurrence(2)
            .select_until(find_position_of("ble").nth_occurrence(3))
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
fn convert_from_use_expression_with_no_parens() {
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
        CONVERT_FROM_USE,
        TestProject::for_source(src),
        find_position_of("use").select_until(find_position_of("wibble")),
    );
}

#[test]
fn convert_from_use_expression_with_empty_parens() {
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
        CONVERT_FROM_USE,
        TestProject::for_source(src),
        find_position_of("use").to_selection(),
    );
}

#[test]
fn convert_from_use_expression_with_parens_and_other_args() {
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
        CONVERT_FROM_USE,
        TestProject::for_source(src),
        find_position_of("wibble").select_until(find_position_of("1")),
    );
}

#[test]
fn convert_from_use_expression_with_single_pattern() {
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
        CONVERT_FROM_USE,
        TestProject::for_source(src),
        find_position_of("a <-").to_selection(),
    );
}

#[test]
fn convert_from_use_expression_with_multiple_patterns() {
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
        CONVERT_FROM_USE,
        TestProject::for_source(src),
        find_position_of("wibble").to_selection(),
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
        CONVERT_FROM_USE,
        TestProject::for_source(src),
        find_position_of("use").nth_occurrence(2).to_selection(),
    );
}

#[test]
fn convert_from_use_only_triggers_on_the_use_line() {
    let src = r#"
pub fn main() {
  use a, b <- wibble(1, 2)
  todo
}

fn wibble(n, m, f) {
    f(1, 2)
}
"#;
    assert_no_code_actions!(
        CONVERT_FROM_USE,
        TestProject::for_source(src),
        find_position_of("todo").to_selection(),
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
        CONVERT_FROM_USE,
        TestProject::for_source(src),
        find_position_of("<-").select_until(find_position_of("wibble")),
    );
}

#[test]
fn convert_from_use_expression_with_type_annotations() {
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
        CONVERT_FROM_USE,
        TestProject::for_source(src),
        find_position_of("<-").select_until(find_position_of("wibble")),
    );
}

#[test]
fn convert_from_use_expression_doesnt_work_with_complex_patterns() {
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
        CONVERT_FROM_USE,
        TestProject::for_source(src),
        find_position_of("<-").select_until(find_position_of("wibble")),
    );
}

#[test]
fn convert_from_use_with_labels() {
    let src = r#"
pub fn main() {
  use a <- wibble(one: 1, two: 2)
  todo
}

fn wibble(one _, two _, three f) {
    f(1)
}
"#;
    assert_code_action!(
        CONVERT_FROM_USE,
        TestProject::for_source(src),
        find_position_of("use").to_selection(),
    );
}

#[test]
fn convert_from_use_with_labels_2() {
    let src = r#"
pub fn main() {
  use a <- wibble(1, two: 2)
  todo
}

fn wibble(one _, two _, three f) {
    f(1)
}
"#;
    assert_code_action!(
        CONVERT_FROM_USE,
        TestProject::for_source(src),
        find_position_of("use").to_selection(),
    );
}

#[test]
fn convert_from_use_with_labels_3() {
    let src = r#"
pub fn main() {
  use a <- wibble(1, three: 3)
  todo
}

fn wibble(one _, two f, three _) {
    f(1)
}
"#;
    assert_code_action!(
        CONVERT_FROM_USE,
        TestProject::for_source(src),
        find_position_of("use").to_selection(),
    );
}

#[test]
fn convert_from_use_with_labels_4() {
    let src = r#"
pub fn main() {
  use a <- wibble(two: 2, three: 3)
  todo
}

fn wibble(one f, two _, three _) {
    f(1)
}
"#;
    assert_code_action!(
        CONVERT_FROM_USE,
        TestProject::for_source(src),
        find_position_of("use").to_selection(),
    );
}

#[test]
// https://github.com/gleam-lang/gleam/issues/4149
fn convert_from_use_with_trailing_comma() {
    let src = r#"
pub fn main() {
  use a, b <- wibble(1, 2,)
  todo
}

fn wibble(n, m, f) {
    f(todo, todo)
}
"#;
    assert_code_action!(
        CONVERT_FROM_USE,
        TestProject::for_source(src),
        find_position_of("<-").select_until(find_position_of("wibble")),
    );
}

#[test]
fn convert_from_use_with_trailing_comma_2() {
    let src = r#"
pub fn main() {
  use a, b <- wibble(
    1,
    2,
  )
  todo
}

fn wibble(n, m, f) {
    f(todo, todo)
}
"#;
    assert_code_action!(
        CONVERT_FROM_USE,
        TestProject::for_source(src),
        find_position_of("<-").select_until(find_position_of("wibble")),
    );
}

#[test]
fn convert_from_use_with_trailing_comma_and_label() {
    let src = r#"
pub fn main() {
  use a, b <- wibble(
    1,
    wibble: 2,
  )
  todo
}

fn wibble(n, wibble m, wobble f) {
    f(todo, todo)
}
"#;
    assert_code_action!(
        CONVERT_FROM_USE,
        TestProject::for_source(src),
        find_position_of("<-").select_until(find_position_of("wibble")),
    );
}

#[test]
fn convert_from_use_multiline_with_no_trailing_comma() {
    let src = r#"
pub fn main() {
  use a, b <- wibble(
    1,
    2
  )
  todo
}

fn wibble(n, m, f) {
    f(todo, todo)
}
"#;
    assert_code_action!(
        CONVERT_FROM_USE,
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
// https://github.com/gleam-lang/gleam/issues/4498
fn turn_call_into_use_with_out_of_order_arguments() {
    assert_code_action!(
        CONVERT_TO_USE,
        r#"
pub fn main() {
  fold(0, over: [], with: fn (a, b) { todo })
}

fn fold(over list: List(a), from acc: acc, with fun: fn(acc, a) -> acc) -> acc {
  todo
}
"#,
        find_position_of("fold").to_selection(),
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
fn extract_variable_does_not_extract_top_level_statement() {
    assert_no_code_actions!(
        EXTRACT_VARIABLE,
        r#"pub fn main() {
  let wibble = 1
}"#,
        find_position_of("1").to_selection()
    );
}

#[test]
fn extract_variable_does_not_extract_top_level_statement_inside_block() {
    assert_no_code_actions!(
        EXTRACT_VARIABLE,
        r#"pub fn main() {
  let x = {
    let y = "y"
    let w = "w" <> y
    w
  }
}"#,
        find_position_of("y").nth_occurrence(2).to_selection()
    );
}

#[test]
fn extract_variable_does_not_extract_top_level_statement_inside_use() {
    assert_no_code_actions!(
        EXTRACT_VARIABLE,
        "
pub fn main() {
  use x <- try(Ok(1))
  let y = 2
  Ok(y + x)
}
pub fn try(result: Result(a, e), fun: fn(a) -> Result(b, e)) -> Result(b, e) { todo }
",
        find_position_of("2").to_selection()
    );
}

#[test]
fn extract_variable_does_not_extract_use() {
    assert_no_code_actions!(
        EXTRACT_VARIABLE,
        "
pub fn main() {
  use x <- try(Ok(1))
  Ok(x)
}
pub fn try(result: Result(a, e), fun: fn(a) -> Result(b, e)) -> Result(b, e) { todo }
",
        find_position_of("use").to_selection()
    );
}

#[test]
fn extract_variable_does_not_extract_panic() {
    assert_no_code_actions!(
        EXTRACT_VARIABLE,
        r#"pub fn main() {
  let x = 1
  panic
}"#,
        find_position_of("panic").to_selection()
    );
}

#[test]
fn extract_variable_does_not_extract_echo() {
    assert_no_code_actions!(
        EXTRACT_VARIABLE,
        r#"pub fn main() {
  let x = 1
  echo x
}"#,
        find_position_of("echo").to_selection()
    );
}

#[test]
fn extract_variable_from_arg_in_pipelined_call() {
    assert_code_action!(
        EXTRACT_VARIABLE,
        "
pub fn main() {
  let adder = add
  let x = [4, 5, 6] |> map2([1, 2, 3], adder)
  x
}
pub fn map2(list1: List(a), list2: List(b), fun: fn(a, b) -> c) -> List(c) { todo }
pub fn add(a: Int, b: Int) -> Int { todo }
",
        find_position_of("[1").to_selection()
    );
}

#[test]
fn extract_variable_from_arg_in_pipelined_call_to_capture() {
    assert_code_action!(
        EXTRACT_VARIABLE,
        "
pub fn main() {
  let adder = add
  let x = adder |> reduce([1, 2, 3], _)
  x
}
pub fn reduce(list: List(a), fun: fn(a, a) -> a) -> Result(a, Nil) { todo }
pub fn add(a: Int, b: Int) -> Int { todo }
",
        find_position_of("[1").to_selection()
    );
}

#[test]
fn extract_variable_from_arg_in_pipelined_call_of_function_to_capture() {
    assert_code_action!(
        EXTRACT_VARIABLE,
        "
pub fn main() {
  fn(total, item) { total + item }
  |> fold(with: _, from: 0, over: [1, 2, 3])
}
pub fn fold(over l: List(a), from i: t, with f: fn(t, a) -> t) -> acc { todo }
",
        find_position_of("fold").to_selection()
    );
}

#[test]
fn extract_variable_from_arg_in_nested_function_called_in_pipeline() {
    assert_code_action!(
        EXTRACT_VARIABLE,
        "
pub fn main() {
  let result =
    [1, 2, 3]
    |> map(add(_, 1))
    |> map(subtract(_, 9))

  result
}
pub fn map(list: List(a), fun: fn(a) -> b) -> List(b) { todo }
pub fn add(a: Int, b: Int) -> Int { todo }
pub fn subtract(a: Int, b: Int) -> Int { todo }
",
        find_position_of("9").to_selection()
    );
}

#[test]
fn extract_variable_does_not_extract_an_entire_pipeline_step() {
    assert_no_code_actions!(
        EXTRACT_VARIABLE,
        "
pub fn main() {
  [1, 2, 3]
  |> map(todo)
  |> map(todo)
}

fn map(list, fun) { todo }
",
        find_position_of("map").to_selection()
    );
}

#[test]
fn extract_variable_does_not_extract_the_last_pipeline_step() {
    assert_no_code_actions!(
        EXTRACT_VARIABLE,
        r#"pub fn main() {
    [1, 2, 3]
    |> map(todo)
    |> map(todo)
}

fn map(list, fun) { todo }
"#,
        find_position_of("map").to_selection()
    );
}

#[test]
fn extract_variable_2() {
    assert_code_action!(
        EXTRACT_VARIABLE,
        "
pub fn main() {
  map([1, 2, 3], add(1, _))
}
pub fn add(n, m) { todo }
pub fn map(l, f) { todo }
",
        find_position_of("add").to_selection()
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
fn extract_variable_in_case_branch_using_var() {
    assert_code_action!(
        EXTRACT_VARIABLE,
        r#"pub fn main() {
  case todo {
    Ok(value) -> 2 * value + 1
    Error(_) -> panic
  }
}"#,
        find_position_of("2").select_until(find_position_of("value").nth_occurrence(2))
    );
}

#[test]
fn extract_variable_in_case_branch_from_second_arg() {
    assert_code_action!(
        EXTRACT_VARIABLE,
        r#"pub fn main() {
  case todo {
    Ok(_) -> #(Ok(1), Error("s"))
    Error(_) -> panic
  }
}"#,
        find_position_of("E").to_selection()
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
fn extract_variable_after_nested_anonymous_function() {
    assert_code_action!(
        EXTRACT_VARIABLE,
        r#"pub fn main() {
    let f = fn() {
        let x = 1 + 2
        let ff = fn() {
            let y = x + 3
            let z = y + x
            z
        }
        let z = x * 4
        z
    }
    let y = 5 + 6
    f()
}"#,
        find_position_of("6").to_selection()
    );
}

#[test]
fn extract_variable_in_nested_anonymous_function() {
    assert_code_action!(
        EXTRACT_VARIABLE,
        r#"pub fn main() {
    let f = fn() {
        let x = 1 + 2
        let ff = fn() {
            let y = x + 3
            let z = y + x
            z
        }
        let z = x * 4
        z
    }
    let y = 5 + 6
    f()
}"#,
        find_position_of("4").to_selection()
    );
}

#[test]
fn extract_variable_in_double_nested_anonymous_function() {
    assert_code_action!(
        EXTRACT_VARIABLE,
        r#"pub fn main() {
    let f = fn() {
        let x = 1 + 2
        let ff = fn() {
            let y = x + 3
            let z = y + x
            z
        }
        let z = x * 4
        z
    }
    let y = 5 + 6
    f()
}"#,
        find_position_of("3").to_selection()
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

#[test]
fn extract_constant_from_call_argument_with_bit_array() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"import gleam/io

pub fn main() {
  io.debug(<<3:size(8)>>)
}"#,
        find_position_of("<").select_until(find_position_of("<").nth_occurrence(2))
    );
}

#[test]
fn extract_constant_from_call_argument_with_float() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"import gleam/float

pub fn main() {
  float.ceiling(1.9998)
}"#,
        find_position_of("1").select_until(find_position_of("8"))
    );
}

#[test]
fn extract_constant_from_call_argument_with_int() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"import gleam/list

pub fn main() {
  list.sample([4, 5, 6], 2)
}"#,
        find_position_of("2").to_selection()
    );
}

#[test]
fn extract_constant_from_call_argument_with_list() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"import gleam/io

pub fn main() {
  io.debug(["constant", "another constant"])
}"#,
        find_position_of("[").to_selection()
    );
}

#[test]
fn extract_constant_from_call_argument_with_nested_inside() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"import gleam/list

pub fn main() {
  list.unzip([#(1, 2), #(3, 4)])
}"#,
        find_position_of("#").select_until(find_position_of("(").nth_occurrence(3))
    );
}

#[test]
fn extract_constant_from_call_argument_with_nested_outside() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"import gleam/list

pub fn main() {
  list.unzip([#(1, 2), #(3, 4)])
}"#,
        find_position_of("[").to_selection()
    );
}

#[test]
fn extract_constant_from_call_argument_with_string() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"import gleam/io

pub fn main() {
  io.print("constant")
}"#,
        find_position_of("\"").select_until(find_position_of("\""))
    );
}

#[test]
fn extract_constant_from_call_argument_with_tuple() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"import gleam/io

pub fn main() {
  io.debug(#(1, 2, 3))
}"#,
        find_position_of("#").select_until(find_position_of("(").nth_occurrence(3))
    );
}

#[test]
fn extract_constant_from_declaration_of_float() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"pub fn main() {
  let c = 3.1415
}"#,
        find_position_of("3").select_until(find_position_of("5"))
    );
}

#[test]
fn extract_constant_from_whole_declaration_of_float() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"import gleam/io

pub fn main() {
  let c = 3.1415
  io.debug(c)
}"#,
        find_position_of("l")
            .nth_occurrence(2)
            .select_until(find_position_of("c"))
    );
}

#[test]
fn extract_constant_from_declaration_of_bit_array() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"pub fn main() {
  let b = <<"arr":utf32>>
}"#,
        find_position_of("u")
            .nth_occurrence(2)
            .select_until(find_position_of(">").nth_occurrence(2))
    );
}

#[test]
fn extract_constant_from_whole_declaration_of_bit_array() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"import gleam/io

const n = 24

pub fn main() {
  let bits = <<8080:size(n)>>
  bits
}"#,
        find_position_of("l")
            .nth_occurrence(2)
            .select_until(find_position_of("s").nth_occurrence(2))
    );
}

#[test]
fn extract_constant_from_declaration_of_bin_op() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"pub fn main() {
  let twelve = "1" <> "2"
}"#,
        find_position_of("<").to_selection()
    );
}

#[test]
fn extract_constant_from_whole_declaration_of_bin_op() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"import gleam/io

pub fn main() {
  let twelve = "1" <> "2"
  io.print(twelve)
}"#,
        find_position_of("l")
            .nth_occurrence(2)
            .select_until(find_position_of("e").nth_occurrence(4))
    );
}

#[test]
fn extract_constant_from_declaration_of_int() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"pub fn main() {
  let c = 125
}"#,
        find_position_of("1").select_until(find_position_of("5"))
    );
}

#[test]
fn extract_constant_from_whole_declaration_of_int() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"import gleam/io

pub fn main() {
  let c = 125
  io.debug(c)
}"#,
        find_position_of("l")
            .nth_occurrence(2)
            .select_until(find_position_of("c"))
    );
}

#[test]
fn extract_constant_from_declaration_of_list() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"pub fn main() {
  let c = [3.1415, 0.33333333]
}"#,
        find_position_of("[").to_selection()
    );
}

#[test]
fn extract_constant_from_whole_declaration_of_list() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"import gleam/io

pub fn main() {
  let c = [3.1415, 0.33333333]
  io.debug(c)
}"#,
        find_position_of("l")
            .nth_occurrence(2)
            .select_until(find_position_of("c"))
    );
}

#[test]
fn extract_constant_from_declaration_of_nested_inside() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"pub fn main() {
  let c = #([1, 2, 3], [3, 2, 1])
}"#,
        find_position_of("[").to_selection()
    );
}

#[test]
fn extract_constant_from_declaration_of_nested_outside() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"pub fn main() {
  let c = #([1, 2, 3], [3, 2, 1])
}"#,
        find_position_of("#").select_until(find_position_of("(").nth_occurrence(2))
    );
}

#[test]
fn extract_constant_from_whole_declaration_of_nested() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"import gleam/io

pub fn main() {
  let c = #([1, 2, 3], [3, 2, 1])
  io.debug(c)
}"#,
        find_position_of("l")
            .nth_occurrence(2)
            .select_until(find_position_of("c"))
    );
}

#[test]
fn extract_constant_from_declaration_of_string() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"pub fn main() {
  let c = "constant"
}"#,
        find_position_of("\"").select_until(find_position_of("\""))
    );
}

#[test]
fn extract_constant_from_whole_declaration_of_string() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"import gleam/io

pub fn main() {
  let c = "constant"
  io.debug(c)
}"#,
        find_position_of("l")
            .nth_occurrence(2)
            .select_until(find_position_of("c"))
    );
}

#[test]
fn extract_constant_from_declaration_of_tuple() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"pub fn main() {
  let #(one, two, three) = #("one", "two", "three")
}"#,
        find_position_of("#")
            .nth_occurrence(2)
            .select_until(find_position_of("(").nth_occurrence(3))
    );
}

#[test]
fn extract_constant_from_whole_declaration_of_tuple() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"import gleam/io

pub fn main() {
  let c = #("one", "two", "three")
  io.debug(c)
}"#,
        find_position_of("l")
            .nth_occurrence(2)
            .select_until(find_position_of("c"))
    );
}

#[test]
fn extract_constant_from_literal_within_list() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"pub fn main() {
  let c = ["constant", todo]
}"#,
        find_position_of("\"").select_until(find_position_of("\""))
    );
}

#[test]
fn extract_constant_from_list_containing_constant() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"const something = "something"

pub fn main() {
  let c = ["constant", something]
}"#,
        find_position_of("[").to_selection()
    );
}

#[test]
fn extract_constant_from_literal_within_tuple() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"pub fn main() {
  let c = #(0.333334, todo)
}"#,
        find_position_of("0").select_until(find_position_of("4"))
    );
}

#[test]
fn extract_constant_from_tuple_containing_constant() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"const something = "something"

pub fn main() {
  let c = #(0.333334, something)
}"#,
        find_position_of("#").select_until(find_position_of("(").nth_occurrence(2))
    );
}

#[test]
fn extract_constant_from_nested_inside_in_expr() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"pub fn main() {
  [#("a", 0), #("b", 1), #("a", 2)]
  |> key_filter("a")
}"#,
        find_position_of("#").select_until(find_position_of("(").nth_occurrence(2))
    );
}

#[test]
fn extract_constant_from_nested_outside_in_expr() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"pub fn main() {
  [#("a", 0), #("b", 1), #("a", 2)]
  |> key_filter("a")
}"#,
        find_position_of("[").to_selection()
    );
}

#[test]
fn extract_constant_from_return_of_float() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"pub fn main() {
  0.25
}"#,
        find_position_of("0").select_until(find_position_of("5"))
    );
}

#[test]
fn extract_constant_from_return_of_int() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"pub fn main() {
  100
}"#,
        find_position_of("1").select_until(find_position_of("0").nth_occurrence(2))
    );
}

#[test]
fn extract_constant_from_return_of_list() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"pub fn main() {
  [1, 2, 3, 4]
}"#,
        find_position_of("[").to_selection()
    );
}

#[test]
fn extract_constant_from_return_of_nested_outside() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"pub fn main() {
  [#(0.25, 0.75), #(0.5, 1.5)]
}"#,
        find_position_of("#").select_until(find_position_of("(").nth_occurrence(2))
    );
}

#[test]
fn extract_constant_from_return_of_string() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"pub fn main() {
  "constant"
}"#,
        find_position_of("\"").select_until(find_position_of("\""))
    );
}

#[test]
fn extract_constant_from_return_of_tuple() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"pub fn main() {
  #(0.25, 0.75)
}"#,
        find_position_of("#").select_until(find_position_of("(").nth_occurrence(2))
    );
}

#[test]
fn extract_constant_from_taken_name_by_function() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"fn floats() {
    [1.0, 2.0]
}

pub fn main() {
  [0.25, 0.75]
}"#,
        find_position_of("[").nth_occurrence(2).to_selection()
    );
}

#[test]
fn extract_constant_from_taken_name_by_constant() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"const ints = [1, 2]

pub fn main() {
  [5, 50]
}"#,
        find_position_of("[").nth_occurrence(2).to_selection()
    );
}

#[test]
fn extract_constant_in_correct_position_1() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"
fn first() {
    1
}

fn second() {
    2
}

fn third() {
    3
}
"#,
        find_position_of("1").to_selection()
    );
}

#[test]
fn extract_constant_in_correct_position_2() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"
fn first() {
    1
}

fn second() {
    2
}

fn third() {
    3
}
"#,
        find_position_of("2").to_selection()
    );
}

#[test]
fn extract_constant_in_correct_position_3() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"
fn first() {
    1
}

fn second() {
    2
}

fn third() {
    3
}
"#,
        find_position_of("3").to_selection()
    );
}

#[test]
fn extract_constant_declaration_with_proper_indentation() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"
pub fn main() {
  let fahrenheit = {
    let degrees = 64
    degrees
  }
  fahrenheit
}
"#,
        find_position_of("l")
            .nth_occurrence(2)
            .select_until(find_position_of("s"))
    );
}

#[test]
fn extract_constant_from_nil() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"pub fn main() {
  let x = Nil
  x
}
"#,
        find_position_of("l").select_until(find_position_of("x"))
    );
}

#[test]
fn extract_constant_from_non_record_variant_1() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"pub type Auth {
  Verified
  Unverified
}

pub fn main() {
  let a = Unverified
  let a = verify(something, a)

  a
}
"#,
        find_position_of("U")
            .nth_occurrence(2)
            .select_until(find_position_of("d").nth_occurrence(3))
    );
}

#[test]
fn extract_constant_from_non_record_variant_2() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"pub type Auth {
  Verified
  Unverified
}

pub fn main() {
  let a = verify(something, Unverified)

  a
}
"#,
        find_position_of("U")
            .nth_occurrence(2)
            .select_until(find_position_of("d").nth_occurrence(3))
    );
}

#[test]
fn extract_constant_from_record_variant_1() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"pub type Auth {
  Verified(String)
  Unverified
}

pub fn main() {
  let u = Verified("User")
  let v = verify(something, u)

  v
}"#,
        find_position_of("l").select_until(find_position_of("u").nth_occurrence(4))
    );
}

#[test]
fn extract_constant_from_record_variant_2() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"pub type Auth {
  Verified(Int)
  Unverified
}

const auth = True

const id = 1234

pub fn main() {
  let v = verify(auth, Verified(id))

  v
}"#,
        find_position_of("V")
            .nth_occurrence(2)
            .select_until(find_position_of("(").nth_occurrence(4))
    );
}

#[test]
fn extract_constant_from_inside_block() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"pub fn main() {
  let fahrenheit = {
    let degrees = 64 + 32
    degrees
  }
}"#,
        find_position_of("32").to_selection()
    );
}

#[test]
fn extract_constant_from_inside_case() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"pub fn main(result) {
  case result {
    Ok(value) -> value + 1
    Error(_) -> panic
  }
}"#,
        find_position_of("1").to_selection()
    );
}

#[test]
fn extract_constant_from_inside_use_1() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"pub fn main() {
  use x <- result.try(todo)
  Ok(123)
}"#,
        find_position_of("Ok").to_selection()
    );
}

#[test]
fn extract_constant_from_inside_use_2() {
    assert_code_action!(
        EXTRACT_CONSTANT,
        r#"const number = 123

pub fn main() {
  use x <- result.try(todo)
  Ok(number)
}"#,
        find_position_of("Ok").to_selection()
    );
}

#[test]
fn do_not_extract_constant_from_pattern() {
    assert_no_code_actions!(
        EXTRACT_CONSTANT,
        r#"pub fn main() {
  let #(one, two) = #(1, 2)
  one
}"#,
        find_position_of("l").select_until(find_position_of("(").nth_occurrence(2))
    );
}

#[test]
fn do_not_extract_constant_from_fn_call_1() {
    assert_no_code_actions!(
        EXTRACT_CONSTANT,
        r#"import gleam/io

pub fn main() {
  io.print("constant")
}"#,
        find_position_of("p")
            .nth_occurrence(3)
            .select_until(find_position_of("t").nth_occurrence(2))
    );
}

#[test]
fn do_not_extract_constant_from_fn_call_2() {
    assert_no_code_actions!(
        EXTRACT_CONSTANT,
        r#"import gleam/list

pub fn main() {
  let first = list.first([1, 2, 3])
  first
}"#,
        find_position_of("l")
            .nth_occurrence(3)
            .select_until(find_position_of("t").nth_occurrence(4))
    );
}

#[test]
fn do_not_extract_constant_from_bin_op() {
    assert_no_code_actions!(
        EXTRACT_CONSTANT,
        r#"pub fn main() {
  let res = 64 < 32
  res
}"#,
        find_position_of("<").to_selection()
    );
}

#[test]
fn do_not_extract_constant_from_bit_array_1() {
    assert_no_code_actions!(
        EXTRACT_CONSTANT,
        r#"pub fn main() {
  let c = "constant"
  let res = <<c:utf16>>
  res
}"#,
        find_position_of("<").to_selection()
    );
}

#[test]
fn do_not_extract_constant_from_bit_array_2() {
    assert_no_code_actions!(
        EXTRACT_CONSTANT,
        r#"import gleam/io

pub fn main() {
  let n = 1234
  io.debug(<<8080:size(n)>>)
}"#,
        find_position_of("<").to_selection()
    );
}

#[test]
fn do_not_extract_constant_from_bit_array_3() {
    assert_no_code_actions!(
        EXTRACT_CONSTANT,
        r#"import gleam/io

pub fn main() {
  let l = 1234
  let r = 1234
  let result = <<l:size(r)>>
  result
}"#,
        find_position_of("l")
            .nth_occurrence(5)
            .select_until(find_position_of("t").nth_occurrence(5))
    );
}

#[test]
fn do_not_extract_constant_from_list_1() {
    assert_no_code_actions!(
        EXTRACT_CONSTANT,
        r#"pub fn main() {
  let c = ["constant", todo]
  c
}"#,
        find_position_of("[").to_selection()
    );
}

#[test]
fn do_not_extract_constant_from_list_2() {
    assert_no_code_actions!(
        EXTRACT_CONSTANT,
        r#"pub fn main() {
  [10, todo]
}"#,
        find_position_of("[").to_selection()
    );
}

#[test]
fn do_not_extract_constant_from_list_3() {
    assert_no_code_actions!(
        EXTRACT_CONSTANT,
        r#"pub fn main() {
  let c = [0.25, todo]
  c
}"#,
        find_position_of("l").select_until(find_position_of("c"))
    );
}

#[test]
fn do_not_extract_constant_from_tuple_1() {
    assert_no_code_actions!(
        EXTRACT_CONSTANT,
        r#"pub fn main() {
  let c = #("constant", todo)
  c
}"#,
        find_position_of("#").select_until(find_position_of("("))
    );
}

#[test]
fn do_not_extract_constant_from_tuple_2() {
    assert_no_code_actions!(
        EXTRACT_CONSTANT,
        r#"pub fn main() {
  #(10, todo)
}"#,
        find_position_of("#").select_until(find_position_of("("))
    );
}

#[test]
fn do_not_extract_constant_from_tuple_3() {
    assert_no_code_actions!(
        EXTRACT_CONSTANT,
        r#"pub fn main() {
  let c = #(0.25, todo)
  c
}"#,
        find_position_of("l").select_until(find_position_of("c"))
    );
}

#[test]
fn do_not_extract_constant_from_nested_1() {
    assert_no_code_actions!(
        EXTRACT_CONSTANT,
        r#"pub fn main() {
  let c = list.unzip([#(1, 2), #(3, todo)])
  c
}"#,
        find_position_of("[").to_selection()
    );
}

#[test]
fn do_not_extract_constant_from_nested_2() {
    assert_no_code_actions!(
        EXTRACT_CONSTANT,
        r#"pub fn main() {
  [[1.25, 1], [0.25, todo]]
}"#,
        find_position_of("[").to_selection()
    );
}

#[test]
fn do_not_extract_constant_from_nested_3() {
    assert_no_code_actions!(
        EXTRACT_CONSTANT,
        r#"import gleam/list

pub fn main() {
  let c = [#(1, 2), #(3, todo)]
  c
}"#,
        find_position_of("l")
            .nth_occurrence(3)
            .select_until(find_position_of("c"))
    );
}

#[test]
fn do_not_extract_constant_from_record_1() {
    assert_no_code_actions!(
        EXTRACT_CONSTANT,
        r#"type Pair {
  Pair(Int, Int)
}

pub fn main() {
  let c = list.unzip([Pair(1, 2), Pair(3, todo)])
  c
}"#,
        find_position_of("P").nth_occurrence(4).to_selection()
    );
}

#[test]
fn do_not_extract_constant_from_record_2() {
    assert_no_code_actions!(
        EXTRACT_CONSTANT,
        r#"type Couple {
  Couple(l: Float, r: Float)
}

pub fn main() {
  #(Couple(1.25, 1.0), Couple(0.25, todo))
}"#,
        find_position_of("C").nth_occurrence(4).to_selection()
    );
}

#[test]
fn do_not_extract_constant_from_record_update() {
    assert_no_code_actions!(
        EXTRACT_CONSTANT,
        r#"type Couple {
  Couple(l: Int, r: Int)
}

pub fn main() {
  let c = Couple(1, 2)
  let cc = Couple(..c, 2)
  cc
}"#,
        find_position_of("C")
            .nth_occurrence(4)
            .select_until(find_position_of("e").nth_occurrence(7))
    );
}

#[test]
fn do_not_extract_constant_from_record_capture() {
    assert_no_code_actions!(
        EXTRACT_CONSTANT,
        r#"type Couple {
  Couple(l: Int, r: Int)
}

pub fn main() {
  let c = Couple(1, _)
  c
}"#,
        find_position_of("C")
            .nth_occurrence(3)
            .select_until(find_position_of("e").nth_occurrence(5))
    );
}

#[test]
fn do_not_extract_top_level_expression_statement() {
    assert_no_code_actions!(
        EXTRACT_VARIABLE,
        r#"pub fn main() {
    1
}
"#,
        find_position_of("1").to_selection()
    );
}

#[test]
fn do_not_extract_top_level_expression_in_let_statement() {
    assert_no_code_actions!(
        EXTRACT_VARIABLE,
        r#"pub fn main() {
    let a = 1
}
"#,
        find_position_of("1").to_selection()
    );
}

#[test]
fn do_not_extract_top_level_module_call() {
    let src = r#"
import list
pub fn main() {
  list.map([1, 2, 3], todo)
}"#;

    assert_no_code_actions!(
        EXTRACT_VARIABLE,
        TestProject::for_source(src).add_module("list", "pub fn map(l, f) { todo }"),
        find_position_of("map").to_selection()
    );
}

#[test]
fn expand_function_capture() {
    assert_code_action!(
        EXPAND_FUNCTION_CAPTURE,
        r#"pub fn main() {
  wibble(_, 1)
}"#,
        find_position_of("_").to_selection()
    );
}

#[test]
fn expand_function_capture_2() {
    assert_code_action!(
        EXPAND_FUNCTION_CAPTURE,
        r#"pub fn main() {
  wibble(1, _)
}"#,
        find_position_of("wibble").to_selection()
    );
}

#[test]
fn expand_function_capture_does_not_shadow_variables() {
    assert_code_action!(
        EXPAND_FUNCTION_CAPTURE,
        r#"pub fn main() {
  let value = 1
  let value_2 = 2
  wibble(value, _, value_2)
}"#,
        find_position_of("wibble").to_selection()
    );
}

#[test]
fn expand_function_capture_picks_a_name_based_on_the_type_of_the_hole() {
    assert_code_action!(
        EXPAND_FUNCTION_CAPTURE,
        r#"pub fn main() {
  [1, 2, 3]
  |> map(add(_, 1))
}

pub fn map(l: List(a), f: fn(a) -> b) -> List(b) { todo }
pub fn add(n, m) { n + m }
"#,
        find_position_of("add").to_selection()
    );
}

#[test]
fn generate_dynamic_decoder() {
    assert_code_action!(
        GENERATE_DYNAMIC_DECODER,
        "
pub type Person {
  Person(name: String, age: Int, height: Float, is_cool: Bool, brain: BitArray)
}
",
        find_position_of("type").to_selection()
    );
}

#[test]
fn generate_dynamic_decoder_complex_types() {
    let src = "
import gleam/option
import gleam/dynamic
import gleam/dict

pub type Something

pub type Wibble(value) {
  Wibble(
    maybe: option.Option(Something),
    map: dict.Dict(String, List(value)),
    unknown: List(dynamic.Dynamic),
  )
}
";

    assert_code_action!(
        GENERATE_DYNAMIC_DECODER,
        TestProject::for_source(src)
            .add_module("gleam/option", "pub type Option(a)")
            .add_module("gleam/dynamic", "pub type Dynamic")
            .add_module("gleam/dict", "pub type Dict(k, v)"),
        find_position_of("type W").to_selection()
    );
}

#[test]
fn generate_dynamic_decoder_already_imported_module() {
    let src = "
import gleam/dynamic/decode as dyn_dec

pub type Wibble {
  Wibble(a: Int, b: Float, c: String)
}
";

    assert_code_action!(
        GENERATE_DYNAMIC_DECODER,
        TestProject::for_source(src).add_module("gleam/dynamic/decode", "pub type Decoder(a)"),
        find_position_of("type W").to_selection()
    );
}

#[test]
fn generate_dynamic_decoder_tuple() {
    assert_code_action!(
        GENERATE_DYNAMIC_DECODER,
        "
pub type Wibble {
  Wibble(tuple: #(Int, Float, #(String, Bool)))
}
",
        find_position_of("type W").to_selection()
    );
}

#[test]
fn generate_dynamic_decoder_recursive_type() {
    let src = "
import gleam/option

pub type LinkedList {
  LinkedList(value: Int, next: option.Option(LinkedList))
}
";
    assert_code_action!(
        GENERATE_DYNAMIC_DECODER,
        TestProject::for_source(src).add_module("gleam/option", "pub type Option(a)"),
        find_position_of("type").to_selection()
    );
}

#[test]
fn generate_dynamic_decoder_for_multi_variant_type() {
    assert_code_action!(
        GENERATE_DYNAMIC_DECODER,
        "
pub type Wibble {
  Wibble(wibble: Int, next: Wibble)
  Wobble(wobble: Float, text: String, values: List(Bool))
}
",
        find_position_of("type").to_selection()
    );
}

#[test]
fn generate_dynamic_decoder_for_multi_variant_type_multi_word_name() {
    assert_code_action!(
        GENERATE_DYNAMIC_DECODER,
        "
pub type Wibble {
  OneTwo(wibble: Int, next: Wibble)
  ThreeFour(wobble: Float, text: String, values: List(Bool))
  FiveSixSeven(one_two: Int)
}
",
        find_position_of("type").to_selection()
    );
}

#[test]
fn generate_dynamic_decoder_for_variant_with_no_fields() {
    assert_code_action!(
        GENERATE_DYNAMIC_DECODER,
        "
pub type Wibble {
  Wibble
}
",
        find_position_of("type").to_selection()
    );
}

#[test]
fn generate_dynamic_decoder_for_variants_with_no_fields() {
    assert_code_action!(
        GENERATE_DYNAMIC_DECODER,
        "
pub type Wibble {
  Wibble
  Wobble
  Woo
}
",
        find_position_of("type").to_selection()
    );
}

#[test]
fn generate_dynamic_decoder_for_variants_with_mixed_fields() {
    assert_code_action!(
        GENERATE_DYNAMIC_DECODER,
        "
pub type Wibble {
  Wibble
  Wobble(field: String, field2: Int)
}
",
        find_position_of("type").to_selection()
    );
}

#[test]
fn no_code_action_to_generate_dynamic_decoder_for_type_without_labels() {
    assert_no_code_actions!(
        GENERATE_DYNAMIC_DECODER,
        "
pub type Wibble {
  Wibble(Int, Int, String)
}
",
        find_position_of("type").to_selection()
    );
}

#[test]
fn pattern_match_on_argument_empty_tuple() {
    assert_no_code_actions!(
        PATTERN_MATCH_ON_ARGUMENT,
        "
pub fn main(tuple: #()) {
  todo
}
",
        find_position_of("tuple").to_selection()
    );
}

#[test]
fn pattern_match_on_argument_single_item_tuple() {
    assert_code_action!(
        PATTERN_MATCH_ON_ARGUMENT,
        "
pub fn main(tuple: #(Int)) {
  todo
}
",
        find_position_of(":").to_selection()
    );
}

#[test]
fn pattern_match_on_argument_multi_item_tuple() {
    assert_code_action!(
        PATTERN_MATCH_ON_ARGUMENT,
        "
pub fn main(tuple: #(Int, String, Bool)) {
  todo
}
",
        find_position_of("tuple").select_until(find_position_of("Int"))
    );
}

#[test]
fn pattern_match_on_argument_uses_case_with_multiple_constructors() {
    assert_code_action!(
        PATTERN_MATCH_ON_ARGUMENT,
        "
pub type CannotBeDestructured {
  One(one: String)
  Two(two: Int)
}

pub fn main(arg: CannotBeDestructured) {
  todo
}
",
        find_position_of("arg").to_selection()
    );
}

#[test]
fn pattern_match_on_argument_with_multiple_constructors_is_nicely_formatted_in_function_with_empty_body()
 {
    assert_code_action!(
        PATTERN_MATCH_ON_ARGUMENT,
        "
pub type CannotBeDestructured {
  One(one: String)
  Two(two: Int)
}

pub fn main(arg: CannotBeDestructured) {}
",
        find_position_of("arg").to_selection()
    );
}

#[test]
fn pattern_match_on_argument_uses_label_shorthand_syntax_for_labelled_arguments() {
    assert_code_action!(
        PATTERN_MATCH_ON_ARGUMENT,
        "
pub type Wibble {
  Wobble(Int, String, i_want_to_see_this: String, and_this: Bool)
}

pub fn main(arg: Wibble) {
  todo
}
",
        find_position_of("arg").select_until(find_position_of("Wibble").nth_occurrence(2))
    );
}

#[test]
fn pattern_match_on_argument_with_private_type_from_same_module() {
    assert_code_action!(
        PATTERN_MATCH_ON_ARGUMENT,
        "
type Wibble {
  Wobble(Int, String)
}

pub fn main(arg: Wibble) {
  todo
}
",
        find_position_of("arg").select_until(find_position_of("Wibble").nth_occurrence(2))
    );
}

#[test]
fn pattern_match_on_value_with_private_type_from_same_module() {
    assert_code_action!(
        PATTERN_MATCH_ON_VARIABLE,
        "
type Wibble {
  Wobble(Int, String)
}

pub fn main() {
  let wibble = Wobble(1, \"Hello\")
  todo
}
",
        find_position_of("wibble").to_selection()
    );
}

#[test]
fn pattern_match_on_argument_will_use_qualified_name() {
    let src = "
import wibble

pub fn main(arg: wibble.Wibble) {
  todo
}
";

    let dep = "
pub type Wibble {
  ThisShouldBeQualified(label: Int)
}
";

    assert_code_action!(
        PATTERN_MATCH_ON_ARGUMENT,
        TestProject::for_source(src).add_module("wibble", dep),
        find_position_of("wibble").nth_occurrence(2).to_selection()
    );
}

#[test]
fn pattern_match_on_argument_will_use_unqualified_name() {
    let src = "
import wibble.{ThisShouldBeUnqualified}

pub fn main(arg: wibble.Wibble) {
  todo
}
";

    let dep = "
pub type Wibble {
  ThisShouldBeUnqualified(label: Int)
}
";

    assert_code_action!(
        PATTERN_MATCH_ON_ARGUMENT,
        TestProject::for_source(src).add_module("wibble", dep),
        find_position_of("Wibble").to_selection()
    );
}

#[test]
fn pattern_match_on_argument_will_use_aliased_constructor_name() {
    let src = "
import wibble.{Wobble as IWantToSeeThisName}

pub fn main(arg: wibble.Wibble) {
  todo
}
";

    let dep = "
pub type Wibble {
  Wobble(label: Int)
}
";

    assert_code_action!(
        PATTERN_MATCH_ON_ARGUMENT,
        TestProject::for_source(src).add_module("wibble", dep),
        find_position_of("arg").to_selection()
    );
}

#[test]
fn pattern_match_on_argument_will_use_aliased_module_name() {
    let src = "
import wibble as i_want_to_see_this_name

pub fn main(arg: i_want_to_see_this_name.Wibble) {
  todo
}
";

    let dep = "
pub type Wibble {
  Wobble(label: Int)
}
";

    assert_code_action!(
        PATTERN_MATCH_ON_ARGUMENT,
        TestProject::for_source(src).add_dep_module("wibble", dep),
        find_position_of("arg").to_selection()
    );
}

#[test]
fn pattern_match_on_argument_not_available_for_internal_type() {
    let src = "
import wibble

pub fn main(arg: wobble.Wibble) {
  todo
}
";

    let dep = "
@internal
pub type Wibble {
  Wobble(label: Int)
}
";

    assert_no_code_actions!(
        PATTERN_MATCH_ON_ARGUMENT,
        TestProject::for_source(src).add_module("wibble", dep),
        find_position_of("arg").to_selection()
    );
}

#[test]
fn pattern_match_on_argument_available_for_internal_type_defined_in_current_module() {
    assert_code_action!(
        PATTERN_MATCH_ON_ARGUMENT,
        "
@internal
pub type Wibble {
  Wobble(label: Int)
}

pub fn main(arg: Wibble) {
  todo
}
",
        find_position_of("arg").select_until(find_position_of("Wibble").nth_occurrence(2))
    );
}

#[test]
fn pattern_match_on_argument_preserves_indentation_of_statement_following_inserted_let() {
    assert_code_action!(
        PATTERN_MATCH_ON_ARGUMENT,
        "pub fn main(arg: #(Int, String)) {
  todo
//^^^^ This should still have two spaces of indentation!
}",
        find_position_of("arg").to_selection()
    );
}

#[test]
fn pattern_match_on_argument_nicely_formats_code_when_used_on_function_with_empty_body() {
    assert_code_action!(
        PATTERN_MATCH_ON_ARGUMENT,
        "pub fn main(arg: #(Int, String)) {}",
        find_position_of("arg").to_selection()
    );
}

#[test]
fn pattern_match_on_argument_single_unlabelled_field_is_not_numbered() {
    assert_code_action!(
        PATTERN_MATCH_ON_ARGUMENT,
        "
pub type Wibble {
  Wibble(Int)
}

pub fn main(arg: Wibble) {}
",
        find_position_of(":").to_selection()
    );
}

#[test]
fn pattern_match_on_let_assignment() {
    assert_code_action!(
        PATTERN_MATCH_ON_VARIABLE,
        "
pub fn main() {
  let var = #(1, 2)
}
",
        find_position_of("var").to_selection()
    );
}

#[test]
fn pattern_match_on_let_assignment_with_multiple_constructors() {
    assert_code_action!(
        PATTERN_MATCH_ON_VARIABLE,
        "
pub type Wibble {
  Wobble
  Woo
}

pub fn main() {
  let var = Woo
  todo
}
",
        find_position_of("var").to_selection()
    );
}

#[test]
fn pattern_match_on_argument_works_on_fn_arguments() {
    assert_code_action!(
        PATTERN_MATCH_ON_ARGUMENT,
        "
pub fn main() {
  [#(1, 2)]
  |> map(fn(tuple) {})
}

fn map(list: List(a), fun: fn(a) -> b) { todo }
",
        find_position_of("tuple").to_selection()
    );
}

#[test]
fn pattern_match_on_argument_works_on_nested_fn_arguments() {
    assert_code_action!(
        PATTERN_MATCH_ON_ARGUMENT,
        "
pub fn main() {
  map([[#(1, 2)]], fn(list) {
    map(list, fn(tuple) {
      todo
    })
  })
}

fn map(list: List(a), fun: fn(a) -> b) { todo }
",
        find_position_of("tuple").to_selection()
    );
}

#[test]
fn generate_function_works_with_invalid_call() {
    assert_code_action!(
        GENERATE_FUNCTION,
        "
pub fn main() -> Bool {
  wibble(1, True, 2.3)
}
",
        find_position_of("wibble").to_selection()
    );
}

#[test]
fn generate_function_works_with_pipeline_steps() {
    assert_code_action!(
        GENERATE_FUNCTION,
        "
pub fn main() {
  [1, 2, 3]
  |> sum
  |> int_to_string
}

fn int_to_string(n: Int) -> String {
  todo
}
",
        find_position_of("sum").to_selection()
    );
}

#[test]
fn generate_function_works_with_pipeline_steps_1() {
    assert_code_action!(
        GENERATE_FUNCTION,
        "
pub fn main() {
  [1, 2, 3]
  |> map(int_to_string)
  |> join
}

fn map(list: List(a), fun: fn(a) -> b) -> List(b) {
  todo
}

fn join(n: List(String)) -> String {
  todo
}
",
        find_position_of("int_to_string").to_selection()
    );
}

// https://github.com/gleam-lang/gleam/issues/4177#event-15968345230
#[test]
fn generate_function_picks_argument_name_based_on_type() {
    assert_code_action!(
        GENERATE_FUNCTION,
        "
pub fn main() {
  wibble(\"Hello\", 1)
}
",
        find_position_of("wibble").to_selection()
    );
}

#[test]
fn generate_function_wont_generate_two_arguments_with_the_same_name_if_they_have_the_same_type() {
    assert_code_action!(
        GENERATE_FUNCTION,
        "
pub fn main() {
  wibble(2, 1)
}
",
        find_position_of("wibble").to_selection()
    );
}

#[test]
fn generate_function_takes_labels_into_account() {
    assert_code_action!(
        GENERATE_FUNCTION,
        "
pub fn main() {
  wibble(2, n: 1)
}
",
        find_position_of("wibble").to_selection()
    );
}

#[test]
fn generate_function_does_not_trigger_if_labels_are_in_the_wrong_order() {
    assert_no_code_actions!(
        GENERATE_FUNCTION,
        "
pub fn main() {
  wibble(n: 2, 1)
}
",
        find_position_of("wibble").to_selection()
    );
}

#[test]
fn generate_function_does_not_trigger_if_there_are_repeated_labels() {
    assert_no_code_actions!(
        GENERATE_FUNCTION,
        "
pub fn main() {
  wibble(n: 2, n: 1)
}
",
        find_position_of("wibble").to_selection()
    );
}

#[test]
fn generate_function_generates_argument_names_from_labels() {
    assert_code_action!(
        GENERATE_FUNCTION,
        "
pub fn main() {
  add(1, addend: 10)
}
",
        find_position_of("add").to_selection()
    );
}

#[test]
fn generate_function_generates_argument_names_from_variables() {
    assert_code_action!(
        GENERATE_FUNCTION,
        "
pub fn main() {
  let wibble = 10
  let wobble = 20

  wubble(wibble, wobble, 14)
}
",
        find_position_of("wubble").to_selection()
    );
}

#[test]
fn generate_function_labels_and_arguments_can_share_the_same_name() {
    assert_code_action!(
        GENERATE_FUNCTION,
        "
pub fn main() {
  let wibble = 10
  wubble(wibble, wibble: 14)
}
",
        find_position_of("wubble").to_selection()
    );
}

#[test]
fn generate_function_arguments_with_same_name_get_renamed() {
    assert_code_action!(
        GENERATE_FUNCTION,
        "
pub fn main() {
  let wibble = 10
  wubble(wibble, wibble)
}
",
        find_position_of("wubble").to_selection()
    );
}

#[test]
fn generate_function_arguments_with_labels_and_variables_uses_different_names() {
    assert_code_action!(
        GENERATE_FUNCTION,
        "
pub fn main() {
  let list = [2, 4, 5]
  let value = 1
  find(each: value, in: list)
}
",
        find_position_of("find").to_selection()
    );
}

#[test]
fn pattern_match_on_argument_generates_unique_names_even_with_labels() {
    assert_code_action!(
        PATTERN_MATCH_ON_ARGUMENT,
        "
pub type Wibble {
  Wibble(String, string: String)
}

pub fn main(wibble: Wibble) {
  todo
}
",
        find_position_of("wibble").to_selection()
    );
}

#[test]
fn extract_variable_with_list_with_plural_name_does_not_add_another_s() {
    assert_code_action!(
        EXTRACT_VARIABLE,
        "
pub fn main() {
  wibble([Names, Names])
}

pub type Names {
  Names
}
",
        find_position_of("[").to_selection()
    );
}

#[test]
fn convert_to_function_call_works_with_argument_in_first_position() {
    assert_code_action!(
        CONVERT_TO_FUNCTION_CALL,
        "
pub fn main() {
  [1, 2, 3]
  |> map(todo)
}

fn map(list: List(a), fun: fn(a) -> b) -> List(b) { todo }
",
        find_position_of("map").to_selection()
    );
}

#[test]
fn generate_json_encoder() {
    let src = "
pub type Person {
  Person(name: String, age: Int, height: Float, is_cool: Bool)
}
";

    assert_code_action!(
        GENERATE_TO_JSON_FUNCTION,
        TestProject::for_source(src).add_package_module(
            "gleam_json",
            "gleam/json",
            "pub type Json"
        ),
        find_position_of("type").to_selection()
    );
}

#[test]
fn convert_to_function_call_works_with_argument_in_first_position_2() {
    assert_code_action!(
        CONVERT_TO_FUNCTION_CALL,
        "
pub fn main() {
  [1, 2, 3] |> wibble
}

fn wibble(a) { todo }
",
        find_position_of("wibble").to_selection()
    );
}

#[test]
fn generate_json_encoder_complex_types() {
    let src = "
import gleam/option
import gleam/dict

pub type Something

pub type Wibble(value) {
  Wibble(
    maybe: option.Option(Int),
    something: Something,
    map: dict.Dict(String, List(Float)),
    unknown: List(value),
  )
}
";

    assert_code_action!(
        GENERATE_TO_JSON_FUNCTION,
        TestProject::for_source(src)
            .add_module("gleam/option", "pub type Option(a)")
            .add_module("gleam/dict", "pub type Dict(k, v)")
            .add_package_module("gleam_json", "gleam/json", "pub type Json"),
        find_position_of("type W").to_selection()
    );
}

#[test]
fn convert_to_function_call_works_with_argument_in_first_position_3() {
    assert_code_action!(
        CONVERT_TO_FUNCTION_CALL,
        "
pub fn main() {
  [1, 2, 3] |> wibble()
}

fn wibble(a) { todo }
",
        find_position_of("wibble").to_selection()
    );
}

#[test]
fn generate_json_encoder_already_imported_module() {
    let src = "
import gleam/json as json_encoding

pub type Wibble {
  Wibble(a: Int, b: Float, c: String)
}
";

    assert_code_action!(
        GENERATE_TO_JSON_FUNCTION,
        TestProject::for_source(src).add_package_module(
            "gleam_json",
            "gleam/json",
            "pub type Json"
        ),
        find_position_of("type W").to_selection()
    );
}

#[test]
fn convert_to_function_call_works_with_argument_in_first_position_4() {
    assert_code_action!(
        CONVERT_TO_FUNCTION_CALL,
        "
pub fn main() {
  [1, 2, 3] |> wibble.wobble
}
",
        find_position_of("wibble").to_selection()
    );
}

#[test]
fn generate_json_encoder_tuple() {
    let src = "
pub type Wibble {
  Wibble(tuple: #(Int, Float, #(String, Bool)))
}
";

    assert_code_action!(
        GENERATE_TO_JSON_FUNCTION,
        TestProject::for_source(src).add_package_module(
            "gleam_json",
            "gleam/json",
            "pub type Json"
        ),
        find_position_of("type W").to_selection()
    );
}

#[test]
fn generate_json_encoder_for_variant_with_no_fields() {
    let src = "
pub type Wibble {
  Wibble
}
";

    assert_code_action!(
        GENERATE_TO_JSON_FUNCTION,
        TestProject::for_source(src).add_package_module(
            "gleam_json",
            "gleam/json",
            "pub type Json"
        ),
        find_position_of("type W").to_selection()
    );
}

#[test]
fn generate_json_encoder_for_type_with_multiple_variants_with_no_fields() {
    let src = "
pub type Wibble {
  Wibble
  Wobble
  Woo
}
";

    assert_code_action!(
        GENERATE_TO_JSON_FUNCTION,
        TestProject::for_source(src).add_package_module(
            "gleam_json",
            "gleam/json",
            "pub type Json"
        ),
        find_position_of("type W").to_selection()
    );
}

#[test]
fn generate_json_encoder_for_variants_with_mixed_fields() {
    let src = "
pub type Wibble {
  Wibble
  Wobble(field: String, field1: Int)
}
";

    assert_code_action!(
        GENERATE_TO_JSON_FUNCTION,
        TestProject::for_source(src).add_package_module(
            "gleam_json",
            "gleam/json",
            "pub type Json"
        ),
        find_position_of("type W").to_selection()
    );
}

#[test]
fn convert_to_function_call_works_with_function_producing_another_function() {
    assert_code_action!(
        CONVERT_TO_FUNCTION_CALL,
        "
pub fn main() {
  1 |> wibble(2)
}

fn wibble(c) -> fn(a) -> Nil {
  fn(_) { Nil }
}
",
        find_position_of("wibble").to_selection()
    );
}

#[test]
fn generate_json_encoder_recursive_type() {
    let src = "
import gleam/option.{Some}

pub type LinkedList {
  LinkedList(value: Int, next: option.Option(LinkedList))
}
";
    assert_code_action!(
        GENERATE_TO_JSON_FUNCTION,
        TestProject::for_source(src)
            .add_module("gleam/option", "pub type Option(a) { Some(a) None }")
            .add_package_module("gleam_json", "gleam/json", "pub type Json"),
        find_position_of("type").to_selection()
    );
}

#[test]
fn convert_to_function_call_works_with_hole_in_first_position() {
    assert_code_action!(
        CONVERT_TO_FUNCTION_CALL,
        "
pub fn main() {
  [1, 2, 3]
  |> map(_, todo)
}

fn map(list: List(a), fun: fn(a) -> b) -> List(b) { todo }
",
        find_position_of("[").to_selection()
    );
}

#[test]
fn generate_json_encoder_list_of_tuples() {
    let src = "
pub type Wibble {
  Wibble(values: List(#(Int, String)))
}
";

    assert_code_action!(
        GENERATE_TO_JSON_FUNCTION,
        TestProject::for_source(src).add_package_module(
            "gleam_json",
            "gleam/json",
            "pub type Json"
        ),
        find_position_of("type").to_selection()
    );
}

#[test]
fn generate_json_encoder_for_multi_variant_type() {
    let src = "
pub type Wibble {
  Wibble(wibble: Int, next: Wibble)
  Wobble(wobble: Float, text: String, values: List(Bool))
}
";

    assert_code_action!(
        GENERATE_TO_JSON_FUNCTION,
        TestProject::for_source(src).add_package_module(
            "gleam_json",
            "gleam/json",
            "pub type Json"
        ),
        find_position_of("type").to_selection()
    );
}

#[test]
fn generate_json_encoder_for_multi_variant_type_multi_word_name() {
    let src = "
pub type Wibble {
  OneTwoThree(wibble: Int, next: Wibble)
  FourFive(wobble: Float, text: String, values: List(Bool))
  SixSevenEight(one_two: Float)
}
";

    assert_code_action!(
        GENERATE_TO_JSON_FUNCTION,
        TestProject::for_source(src).add_package_module(
            "gleam_json",
            "gleam/json",
            "pub type Json"
        ),
        find_position_of("type").to_selection()
    );
}

#[test]
fn convert_to_function_call_works_with_hole_not_in_first_position() {
    assert_code_action!(
        CONVERT_TO_FUNCTION_CALL,
        "
pub fn main() {
  fn(a) { todo }
  |> map([1, 2, 3], _)
}

fn map(list: List(a), fun: fn(a) -> b) -> List(b) { todo }
",
        find_position_of("fn(a)").select_until(find_position_of("map"))
    );
}

#[test]
fn convert_to_function_call_always_inlines_the_first_step() {
    assert_code_action!(
        CONVERT_TO_FUNCTION_CALL,
        "
pub fn main() {
  [1, 2, 3]
  |> map(todo)
  |> filter(todo)
}

fn map(list: List(a), fun: fn(a) -> b) -> List(b) { todo }
fn filter(list: List(a), fun: fn(a) -> Bool) -> List(b) { todo }
",
        find_position_of("[1, 2, 3]").select_until(find_position_of("map"))
    );
}

#[test]
fn convert_to_function_call_works_with_labelled_argument() {
    assert_code_action!(
        CONVERT_TO_FUNCTION_CALL,
        "
pub fn main() {
  [1, 2, 3] |> wibble(wobble: _, woo:)
}
",
        find_position_of("wibble").to_selection()
    );
}

#[test]
fn convert_to_function_call_works_with_labelled_argument_2() {
    assert_code_action!(
        CONVERT_TO_FUNCTION_CALL,
        "
pub fn main() {
  [1, 2, 3] |> wibble(wobble:, woo: _)
}
",
        find_position_of("wibble").to_selection()
    );
}

#[test]
fn convert_to_function_call_works_when_piping_an_invalid_module_select() {
    assert_code_action!(
        CONVERT_TO_FUNCTION_CALL,
        "
pub fn main() {
  wibble.wobble |> woo(_)
}
",
        find_position_of("woo").to_selection()
    );
}

#[test]
fn convert_to_function_call_works_when_piping_a_module_select() {
    let src = "
import wibble

pub fn main() {
  wibble.wobble |> woo(_)
}

fn woo(n) { todo }
";

    assert_code_action!(
        CONVERT_TO_FUNCTION_CALL,
        TestProject::for_source(src).add_module("wibble", "pub const wobble = 1"),
        find_position_of("woo").to_selection()
    );
}

#[test]
fn convert_to_function_call_works_with_echo() {
    assert_code_action!(
        CONVERT_TO_FUNCTION_CALL,
        "
pub fn main() {
  wibble.wobble |> echo
}
",
        find_position_of("echo").to_selection()
    );
}

#[test]
fn no_code_action_to_generate_json_encoder_for_type_without_labels() {
    let src = "
pub type Wibble {
  Wibble(Int, Int, String)
}
    ";

    assert_no_code_actions!(
        GENERATE_TO_JSON_FUNCTION,
        TestProject::for_source(src).add_package_module(
            "gleam_json",
            "gleam/json",
            "pub type Json"
        ),
        find_position_of("type").to_selection()
    );
}

#[test]
fn no_code_action_to_generate_json_encoder_without_gleam_json_dependency() {
    assert_no_code_actions!(
        GENERATE_TO_JSON_FUNCTION,
        "
pub type Wibble {
  Wibble(w: Int)
}
",
        find_position_of("type").to_selection()
    );
}

#[test]
fn inline_variable() {
    let src = r#"
import gleam/io

pub fn main() {
  let message = "Hello!"
  io.println(message)
}
"#;
    assert_code_action!(
        INLINE_VARIABLE,
        TestProject::for_source(src).add_module("gleam/io", "pub fn println(value) {}"),
        find_position_of("message)").to_selection()
    );
}

#[test]
fn inline_variable_from_definition() {
    let src = r#"
import gleam/io

pub fn main() {
  let message = "Hello!"
  io.println(message)
}
"#;
    assert_code_action!(
        INLINE_VARIABLE,
        TestProject::for_source(src).add_module("gleam/io", "pub fn println(value) {}"),
        find_position_of("message =").to_selection()
    );
}

#[test]
fn inline_variable_in_nested_scope() {
    let src = r#"
import gleam/io

pub fn main() {
  let _ = {
    let message = "Hello!"
    io.println(message)
  }
}
"#;
    assert_code_action!(
        INLINE_VARIABLE,
        TestProject::for_source(src).add_module("gleam/io", "pub fn println(value) {}"),
        find_position_of("message =").to_selection()
    );
}

#[test]
fn inline_variable_in_case_scope() {
    let src = r#"
import gleam/io

pub fn main(x) {
  case x {
    True -> {
      let message = "Hello!"
      io.println(message)
    }
    False -> Nil
  }
}
"#;
    assert_code_action!(
        INLINE_VARIABLE,
        TestProject::for_source(src).add_module("gleam/io", "pub fn println(value) {}"),
        find_position_of("message =").to_selection()
    );
}

#[test]
fn no_code_action_to_inline_variable_used_multiple_times() {
    let src = r#"
import gleam/io

pub fn main() {
  let message = "Hello!"
  io.println(message)
  io.debug(message)
}
"#;
    assert_no_code_actions!(
        INLINE_VARIABLE,
        TestProject::for_source(src).add_module(
            "gleam/io",
            "pub fn println(value) {} pub fn debug(value) {}"
        ),
        find_position_of("message =").to_selection()
    );
}

#[test]
fn no_code_action_to_inline_variable_defined_in_complex_pattern() {
    let src = r#"
import gleam/io

pub fn main() {
  let #(message, second, _) = todo
  io.println(message)
}
"#;
    assert_no_code_actions!(
        INLINE_VARIABLE,
        TestProject::for_source(src).add_module("gleam/io", "pub fn println(value) {}"),
        find_position_of("message)").to_selection()
    );
}

#[test]
fn no_code_action_to_inline_variable_defined_in_case_clause() {
    let src = r#"
import gleam/io

pub fn main(result) {
  case result {
    Ok(value) -> value
    Error(message) -> {
      io.println(message)
      panic
    }
  }
}
"#;
    assert_no_code_actions!(
        INLINE_VARIABLE,
        TestProject::for_source(src).add_module("gleam/io", "pub fn println(value) {}"),
        find_position_of("message").nth_occurrence(2).to_selection()
    );
}

#[test]
fn convert_to_pipe_with_function_call_on_first_argument() {
    assert_code_action!(
        CONVERT_TO_PIPE,
        "
pub fn main() {
  wibble(wobble, woo)
}
",
        find_position_of("wobble").to_selection()
    );
}

#[test]
fn convert_to_pipe_with_function_call_on_second_argument() {
    assert_code_action!(
        CONVERT_TO_PIPE,
        "
pub fn main() {
  wibble(wobble, woo)
}
",
        find_position_of("woo").to_selection()
    );
}

#[test]
fn convert_to_pipe_with_function_call_on_function_name_extracts_first_argument() {
    assert_code_action!(
        CONVERT_TO_PIPE,
        "
pub fn main() {
  wibble(wobble, woo)
}
",
        find_position_of("wibble").to_selection()
    );
}

#[test]
fn convert_to_pipe_with_function_call_with_labelled_arguments_inserts_hole() {
    assert_code_action!(
        CONVERT_TO_PIPE,
        "
pub fn main() {
  wibble(wobble: 1, woo: 2)
}
",
        find_position_of("wobble").to_selection()
    );
}

#[test]
fn convert_to_pipe_with_function_call_with_labelled_arguments_inserts_hole_2() {
    assert_code_action!(
        CONVERT_TO_PIPE,
        "
pub fn main() {
  wibble(wobble: 1, woo: 2)
}
",
        find_position_of("woo").to_selection()
    );
}

#[test]
fn convert_to_pipe_with_function_call_with_shorthand_labelled_argument_inserts_hole() {
    assert_code_action!(
        CONVERT_TO_PIPE,
        "
pub fn main() {
  wibble(wobble:, woo:)
}
",
        find_position_of("wobble").to_selection()
    );
}

#[test]
fn convert_to_pipe_with_function_call_with_shorthand_labelled_argument_inserts_hole_2() {
    assert_code_action!(
        CONVERT_TO_PIPE,
        "
pub fn main() {
  wibble(wobble:, woo:)
}
",
        find_position_of("woo").to_selection()
    );
}

#[test]
fn convert_to_pipe_on_first_step_of_pipeline() {
    assert_code_action!(
        CONVERT_TO_PIPE,
        "
pub fn main() {
  wibble(wobble, woo) |> wobble
}
",
        find_position_of("wibble").to_selection()
    );
}

#[test]
fn convert_to_pipe_not_allowed_on_other_pipeline_steps() {
    assert_no_code_actions!(
        CONVERT_TO_PIPE,
        "
pub fn main() {
  wibble(wobble) |> wobble(woo)
}
",
        find_position_of("woo").to_selection()
    );
}

#[test]
fn convert_to_pipe_with_function_returning_other_function() {
    assert_code_action!(
        CONVERT_TO_PIPE,
        "
pub fn main() {
  wibble(wobble)(woo)
}
",
        find_position_of("woo").to_selection()
    );
}

#[test]
fn convert_to_pipe_does_not_work_on_function_on_the_right_hand_side_of_use() {
    assert_no_code_actions!(
        CONVERT_TO_PIPE,
        "
pub fn main() {
  use <- wibble(wobble)
  todo
}
",
        find_position_of("wibble").to_selection()
    );
}

#[test]
fn convert_to_pipe_does_not_work_on_function_on_the_right_hand_side_of_use_2() {
    assert_no_code_actions!(
        CONVERT_TO_PIPE,
        "
pub fn main() {
  use <- wibble(wobble)
  todo
}
",
        find_position_of("todo").to_selection()
    );
}

#[test]
fn convert_to_pipe_does_not_work_on_function_with_capture() {
    assert_no_code_actions!(
        CONVERT_TO_PIPE,
        "import gleam/int

pub fn main() {
  let sum = int.add(1, _)
  sum
}
",
        find_position_of("1").to_selection()
    );
}

#[test]
fn convert_to_pipe_does_not_work_on_record_with_capture() {
    assert_no_code_actions!(
        CONVERT_TO_PIPE,
        "pub fn main() {
  Ok(_)
}
",
        find_position_of("O").to_selection()
    );
}

#[test]
fn convert_to_pipe_works_inside_body_of_use() {
    assert_code_action!(
        CONVERT_TO_PIPE,
        "
pub fn main() {
  use <- wibble(wobble)
  woo(123)
}
",
        find_position_of("woo").to_selection()
    );
}

#[test]
fn convert_to_pipe_pipes_the_outermost_argument() {
    assert_code_action!(
        CONVERT_TO_PIPE,
        "
pub fn main() {
  wibble(wobble(woo))
}
",
        find_position_of("wobble").to_selection()
    );
}

#[test]
fn convert_to_pipe_when_first_arg_is_a_pipe_itself() {
    assert_code_action!(
        CONVERT_TO_PIPE,
        "
pub fn main() {
  wibble(wobble |> woo, waa)
}
",
        find_position_of("wibble").to_selection()
    );
}

#[test]
fn convert_to_pipe_with_string_concat_adds_braces() {
    assert_code_action!(
        CONVERT_TO_PIPE,
        "
pub fn main() {
  wibble(wobble <> woo, waa)
}
",
        find_position_of("wibble").to_selection()
    );
}

#[test]
fn convert_to_pipe_with_bool_operator_adds_braces() {
    assert_code_action!(
        CONVERT_TO_PIPE,
        "
pub fn main() {
  wibble(wobble != woo, waa)
}
",
        find_position_of("wibble").to_selection()
    );
}

#[test]
fn convert_to_pipe_with_sum_adds_no_braces() {
    assert_code_action!(
        CONVERT_TO_PIPE,
        "
pub fn main() {
  wibble(1 + 1, waa)
}
",
        find_position_of("wibble").to_selection()
    );
}

#[test]
fn convert_to_pipe_with_comparison_adds_braces() {
    assert_code_action!(
        CONVERT_TO_PIPE,
        "
pub fn main() {
  wibble(1.0 >=. 0.0, waa)
}
",
        find_position_of("wibble").to_selection()
    );
}

#[test]
fn convert_to_pipe_with_complex_binop_adds_braces() {
    assert_code_action!(
        CONVERT_TO_PIPE,
        "
fn bug() {
    bool.guard(1 == 2 || 2 == 3, Nil, fn() { Nil })
}
",
        find_position_of("||").to_selection()
    );
}

// https://github.com/gleam-lang/gleam/issues/4342
#[test]
fn inline_variable_in_record_update() {
    assert_code_action!(
        INLINE_VARIABLE,
        "
type Couple {
  Couple(l: Int, r: Int)
}

pub fn main() {
  let c1 = Couple(l: 1, r: 1)
  let c2 = Couple(..c1, r: 1)
}
",
        find_position_of("c1,").to_selection()
    );
}

// https://github.com/gleam-lang/gleam/issues/4430
#[test]
fn inline_variable_with_record_field() {
    assert_code_action!(
        INLINE_VARIABLE,
        "
type Couple {
  Couple(l: Int, r: Int)
}

pub fn main() {
  let c1 = Couple(l: 1, r: 1)
  let c2 = c1.l
  echo c2
}
",
        find_position_of("c2").nth_occurrence(2).to_selection()
    );
}

#[test]
fn wrap_case_clause_in_block() {
    assert_code_action!(
        WRAP_IN_BLOCK,
        "
pub fn f(option) {
  case option {
    Some(content) -> content
    None -> panic
  }
}",
        find_position_of("content").nth_occurrence(2).to_selection()
    );
}

#[test]
fn wrap_nested_case_clause_in_block() {
    assert_code_action!(
        WRAP_IN_BLOCK,
        "
pub fn f(result) {
  case result {
    Ok(reresult) -> {
      case reresult {
        Ok(w) -> w
        Error(_) -> panic
      }
    }
    Error(_) -> panic
  }
}",
        find_position_of("w").nth_occurrence(2).to_selection()
    );
}

#[test]
fn wrap_case_clause_with_guard_in_block() {
    assert_code_action!(
        WRAP_IN_BLOCK,
        "
pub fn f(option) {
  case option {
    Some(integer) if integer > 0 -> integer
    Some(integer) -> 0
    None -> panic
  }
}",
        find_position_of("integer").nth_occurrence(3).to_selection()
    );
}

#[test]
fn wrap_case_clause_with_multiple_patterns_in_block() {
    assert_code_action!(
        WRAP_IN_BLOCK,
        "pub type PokemonType {
  Air
  Water
  Fire
}

  pub fn f(pokemon_type: PokemonType) {
    case pokemon_type {
      Water | Air -> soak()
      Fire -> burn()
    }
  }",
        find_position_of("soak").to_selection()
    );
}

#[test]
fn wrap_case_clause_inside_assignment_in_block() {
    assert_code_action!(
        WRAP_IN_BLOCK,
        r#"pub type PokemonType {
  Air
  Water
  Fire
}

  pub fn f(pokemon_type: PokemonType) {
    let damage = case pokemon_type {
      Water -> soak()
      Fire -> burn()
    }

    "Pokemon did " <> damage
  }"#,
        find_position_of("burn").to_selection()
    );
}

#[test]
fn wrap_case_assignment_of_record_access_in_block() {
    assert_code_action!(
        WRAP_IN_BLOCK,
        r#"
pub type Record {
  R(left: Int, right: Int)
}

pub fn main() {
  let r = R(1, 2)
  let l = r.left
  l
}
"#,
        find_position_of("left").nth_occurrence(2).to_selection()
    );
}

#[test]
fn do_not_wrap_case_clause_in_block_1() {
    assert_no_code_actions!(
        WRAP_IN_BLOCK,
        "
pub fn f(option) {
  case option {
    Some(content) -> {
      content
    }
    None -> panic
  }
}",
        find_position_of("content").nth_occurrence(2).to_selection()
    );
}

#[test]
fn do_not_wrap_case_clause_in_block_2() {
    assert_no_code_actions!(
        WRAP_IN_BLOCK,
        "
pub fn f(result) {
  case result {
    Ok(reresult) -> {
      case reresult {
        Ok(w) -> {
          w
        }
        Error(_) -> panic
      }
    }
    Error(_) -> panic
  }
}",
        find_position_of("w").nth_occurrence(2).to_selection()
    );
}

#[test]
fn do_not_wrap_case_clause_in_block_3() {
    assert_no_code_actions!(
        WRAP_IN_BLOCK,
        "
pub fn f(option) {
  case option {
    Some(content) -> content
    None -> panic
  }
}",
        find_position_of("Some(content)").to_selection()
    );
}

#[test]
fn wrap_assignment_value_in_block() {
    assert_code_action!(
        WRAP_IN_BLOCK,
        r#"pub fn main() {
  let var = "value"
}"#,
        find_position_of("value").select_until(find_position_of("e").nth_occurrence(2))
    );
}

#[test]
fn do_not_wrap_assignment_value_in_block() {
    assert_no_code_actions!(
        WRAP_IN_BLOCK,
        r#"pub fn main() {
  let var = "value"
}"#,
        find_position_of("var").to_selection()
    );
}

// https://github.com/gleam-lang/gleam/issues/4427
#[test]
fn extract_constant_function() {
    assert_no_code_actions!(
        EXTRACT_CONSTANT,
        r#"
fn print(x) {
  Nil
}

pub fn main() {
  print("Hello")
}
"#,
        find_position_of("print").nth_occurrence(2).to_selection()
    );
}

#[test]
fn fix_float_operator_on_ints() {
    let name = "Use `>=`";
    assert_code_action!(
        name,
        r#"
pub fn main() {
  1 >=. 2
}
"#,
        find_position_of("1").to_selection()
    );
}

#[test]
fn fix_float_operator_on_ints_2() {
    let name = "Use `-`";
    assert_code_action!(
        name,
        r#"
pub fn main() {
  1 -. 2
}
"#,
        find_position_of("1").select_until(find_position_of("2"))
    );
}

#[test]
fn fix_float_operator_on_ints_3() {
    let name = "Use `*`";
    assert_code_action!(
        name,
        r#"
pub fn main() {
  1 *. wobble()
}

fn wobble() { 3 }
"#,
        find_position_of("*.").to_selection()
    );
}

#[test]
fn fix_int_operator_on_floats() {
    let name = "Use `>=.`";
    assert_code_action!(
        name,
        r#"
pub fn main() {
  1.0 >= 2.3
}
"#,
        find_position_of("1").to_selection()
    );
}

#[test]
fn fix_int_operator_on_floats_2() {
    let name = "Use `-.`";
    assert_code_action!(
        name,
        r#"
pub fn main() {
  1.12 - 2.0
}
"#,
        find_position_of("1").select_until(find_position_of("2.0"))
    );
}

#[test]
fn fix_int_operator_on_floats_3() {
    let name = "Use `*.`";
    assert_code_action!(
        name,
        r#"
pub fn main() {
  1.3 * wobble()
}

fn wobble() { 3.2 }
"#,
        find_position_of("*").to_selection()
    );
}

#[test]
fn fix_plus_operator_on_strings() {
    let name = "Use `<>`";
    assert_code_action!(
        name,
        r#"
pub fn main() {
  "hello, " + name()
}

fn name() { "Jak" }
"#,
        find_position_of("hello").select_until(find_position_of("name()"))
    );
}

// https://github.com/gleam-lang/gleam/issues/4454
#[test]
fn unqualify_already_imported_type() {
    let src = "
import wibble.{type Wibble}

pub fn main() -> wibble.Wibble {
  todo
}
";

    assert_code_action!(
        "Unqualify wibble.Wibble",
        TestProject::for_source(src).add_hex_module("wibble", "pub type Wibble"),
        find_position_of("wibble.Wibble").to_selection(),
    );
}

#[test]
fn fill_labels_pattern_constructor() {
    assert_code_action!(
        FILL_LABELS,
        "
pub type Wibble {
  Wibble(a: Int, b: Float, c: String)
  Wobble(d: Bool, e: BitArray, f: List(Result(String, Nil)))
}

pub fn main(w: Wibble) {
  case w {
    Wibble(..) -> todo
    Wobble() -> todo
  }
}
",
        find_position_of("Wobble()").to_selection(),
    );
}

#[test]
fn fill_labels_pattern_constructor_let_assignment() {
    assert_code_action!(
        FILL_LABELS,
        "
pub type Wibble {
  Wibble(a: Int, b: Float, c: String)
}

pub fn main() {
  let Wibble() = todo
}
",
        find_position_of("Wibble()").to_selection(),
    );
}

#[test]
fn fill_labels_pattern_constructor_with_some_labels() {
    assert_code_action!(
        FILL_LABELS,
        "
pub type Wibble {
  Wibble(a: Int, b: Float, c: String)
  Wobble(d: Bool, e: BitArray, f: List(Result(String, Nil)))
}

pub fn main(w: Wibble) {
  case w {
    Wobble(e: <<>>) -> todo
    _ -> todo
  }
}
",
        find_position_of("Wobble(e").to_selection(),
    );
}

#[test]
fn fill_labels_nested_pattern_constructor() {
    assert_code_action!(
        FILL_LABELS,
        "
pub type Wibble {
  Wibble(a: Int, b: Float, c: String)
  Wobble(d: Bool, e: BitArray, f: List(Result(String, Nil)))
}

pub fn main() {
  case todo {
    #([Wobble()], 2, 3) -> todo
    _ -> todo
  }
}
",
        find_position_of("Wobble()").to_selection(),
    );
}

#[test]
// https://github.com/gleam-lang/gleam/issues/4499
fn fill_labels_with_function_with_unlabelled_arguments() {
    assert_no_code_actions!(
        FILL_LABELS,
        "
pub fn main() {
    fold(0, over: [], with: fn(acc, item) { acc + item })
}

pub fn fold(over list, from initial, with fun) { todo }",
        find_position_of("fold").to_selection(),
    );
}

#[test]
fn add_missing_patterns_with_labels() {
    assert_code_action!(
        ADD_MISSING_PATTERNS,
        "
pub type Wibble {
  Wibble(integer: Int, float: Float)
  Wobble(string: String, bool: Bool)
}

pub fn main(w: Wibble) {
  case w {}
}
",
        find_position_of("case w").select_until(find_position_of("{}")),
    );
}

// https://github.com/gleam-lang/gleam/issues/3628#issuecomment-2543342212
#[test]
fn add_missing_patterns_multibyte_grapheme() {
    assert_code_action!(
        ADD_MISSING_PATTERNS,
        r#"
// ä
fn wibble() {
  case True {}
}
"#,
        find_position_of("case").select_until(find_position_of("True {"))
    );
}

// https://github.com/gleam-lang/gleam/issues/4626
#[test]
fn add_missing_patterns_opaque_type() {
    let src = "
import mod

pub fn main(w: mod.Wibble) {
  case w {}
}
";

    assert_code_action!(
        ADD_MISSING_PATTERNS,
        TestProject::for_source(src).add_hex_module(
            "mod",
            "pub opaque type Wibble { Wibble(Int) Wobble(String) }"
        ),
        find_position_of("{}").to_selection(),
    );
}

// https://github.com/gleam-lang/gleam/issues/4653
#[test]
fn generate_function_capture() {
    assert_code_action!(
        GENERATE_FUNCTION,
        "
fn map(list: List(a), f: fn(a) -> b) -> List(b) {
  todo
}

pub fn main() {
  map([1, 2, 3], add(_, 1))
}
",
        find_position_of("add").to_selection()
    );
}
