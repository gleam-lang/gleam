use super::*;
use lsp_types::{
    ParameterInformation, ParameterLabel, SignatureHelp, SignatureHelpParams, SignatureInformation,
};

fn signature_help(tester: TestProject<'_>, position: Position) -> Option<SignatureHelp> {
    tester.at(position, |engine, param, _| {
        let params = SignatureHelpParams {
            context: None,
            text_document_position_params: param,
            work_done_progress_params: Default::default(),
        };
        let response = engine.signature_help(params);

        response.result.unwrap()
    })
}

fn pretty_signature_help(signature_help: SignatureHelp) -> String {
    let SignatureHelp {
        signatures,
        active_signature,
        active_parameter,
    } = signature_help;

    let SignatureInformation {
        label,
        documentation,
        parameters,
        active_parameter: _,
    } = signatures
        .get(active_signature.expect("an active signature") as usize)
        .expect("an active signature");

    let parameters = parameters
        .as_ref()
        .expect("no signature help for function with no parameters");

    let documentation = match documentation {
        Some(d) => format!("Documentation:\n{d:#?}"),
        None => "No documentation".to_string(),
    };

    let label = match active_parameter {
        None => label.to_string(),
        Some(i) => match parameters.get(i as usize) {
            None => label.to_string(),
            Some(ParameterInformation {
                label: ParameterLabel::LabelOffsets([start, end]),
                ..
            }) => {
                let spaces = " ".repeat(*start as usize);
                let underlined = "▔".repeat((end - start) as usize);
                format!("{label}\n{spaces}{underlined}")
            }
            Some(_) => panic!("unexpected response"),
        },
    };

    format!("{label}\n\n{documentation}")
}

#[macro_export]
macro_rules! assert_signature_help {
    ($code:literal, $position:expr $(,)?) => {
        let project = TestProject::for_source($code);
        assert_signature_help!(project, $position);
    };

    ($project:expr, $position:expr $(,)?) => {
        let src = $project.src;
        let position = $position.find_position(src);
        let result = signature_help($project, position).expect("no signature help produced");
        let pretty_hover = hover::show_hover(
            src,
            lsp_types::Range {
                start: Position {
                    character: 1,
                    line: 1,
                },
                end: Position {
                    character: 0,
                    line: 0,
                },
            },
            position,
        );
        let output = format!(
            "{}\n\n----- Signature help -----\n{}",
            pretty_hover,
            pretty_signature_help(result)
        );
        insta::assert_snapshot!(insta::internals::AutoName, output, src);
    };
}

#[macro_export]
macro_rules! assert_no_signature_help {
    ($code:literal, $position:expr $(,)?) => {
        let project = TestProject::for_source($code);
        assert_no_signature_help!(project, $position);
    };

    ($project:expr, $position:expr $(,)?) => {
        let src = $project.src;
        let position = $position.find_position(src);
        let result = signature_help($project, position);
        match result {
            Some(_) => panic!("Expected no signature help"),
            None => (),
        }
    };
}

#[test]
pub fn help_for_calling_local_variable_first_arg() {
    assert_signature_help!(
        r#"
pub fn main() {
  let wibble = fn(a: Int, b: String) { 1.0 }
  wibble()
}
"#,
        find_position_of("wibble()").under_last_char()
    );
}

#[test]
pub fn help_for_calling_local_variable_last_arg() {
    assert_signature_help!(
        r#"
pub fn main() {
  let wibble = fn(a: Int, b: String) { 1.0 }
  wibble(1,)
}
"#,
        find_position_of("wibble(1,)").under_last_char()
    );
}

#[test]
pub fn help_for_calling_local_variable_with_module_function() {
    assert_signature_help!(
        r#"
pub fn wibble(a: Int, b: String) { 1.0 }

pub fn main() {
  let wobble = fn(a: Int, b: String) { 1.0 }
  wobble(1,)
}
"#,
        find_position_of("wobble(1,)").under_last_char()
    );
}

#[test]
pub fn help_for_calling_module_function() {
    assert_signature_help!(
        r#"
pub fn wibble(a: Int, b: String) { 1.0 }

pub fn main() {
  wibble()
}
"#,
        find_position_of("wibble()").under_last_char()
    );
}

#[test]
pub fn help_for_calling_module_constant_referencing_function() {
    assert_signature_help!(
        r#"
pub fn wibble(a: Int, b: String) { 1.0 }
const wobble = wibble

pub fn main() {
  wobble()
}
"#,
        find_position_of("wobble()").under_last_char()
    );
}

#[test]
pub fn help_for_calling_local_variable_referencing_constant_referencing_function() {
    assert_signature_help!(
        r#"
pub fn wibble(a: Int, b: String) { 1.0 }
const wobble = wibble

pub fn main() {
  let woo = wobble
  woo()
}
"#,
        find_position_of("woo()").under_last_char()
    );
}

#[test]
pub fn help_still_shows_up_even_if_an_argument_has_the_wrong_type() {
    assert_signature_help!(
        r#"
pub fn wibble(a: Int, b: String) { 1.0 }

pub fn main() {
  wibble("wrong",)
}
"#,
        find_position_of("wibble(\"wrong\",)").under_last_char()
    );
}

#[test]
pub fn help_shows_documentation_for_local_function() {
    assert_signature_help!(
        r#"
/// Some doc!
pub fn wibble(a: Int, b: String) { 1.0 }

pub fn main() {
  wibble()
}
"#,
        find_position_of("wibble()").under_last_char()
    );
}

#[test]
pub fn help_shows_documentation_for_imported_function() {
    let code = r#"
import example
pub fn main() {
  example.example_fn()
}
"#;
    assert_signature_help!(
        TestProject::for_source(code).add_module(
            "example",
            "/// Some doc!
pub fn example_fn(a: Int, b: String) { Nil }"
        ),
        find_position_of("example_fn()").under_last_char()
    );
}

#[test]
pub fn help_for_unqualified_call() {
    let code = r#"
import example.{example_fn}
pub fn main() {
  example_fn()
}
"#;

    assert_signature_help!(
        TestProject::for_source(code)
            .add_module("example", "pub fn example_fn(a: Int, b: String) { Nil }"),
        find_position_of("example_fn()").under_last_char()
    );
}

#[test]
pub fn help_for_aliased_unqualified_call() {
    let code = r#"
import example.{example_fn as wibble}
pub fn main() {
  wibble()
}
"#;

    assert_signature_help!(
        TestProject::for_source(code)
            .add_module("example", "pub fn example_fn(a: Int, b: String) { Nil }"),
        find_position_of("wibble()").under_last_char()
    );
}

#[test]
pub fn help_for_qualified_call() {
    let code = r#"
import example
pub fn main() {
  example.example_fn()
}
"#;

    assert_signature_help!(
        TestProject::for_source(code)
            .add_module("example", "pub fn example_fn(a: Int, b: String) { Nil }"),
        find_position_of("example_fn()").under_last_char()
    );
}

#[test]
pub fn help_for_aliased_qualified_call() {
    let code = r#"
import example as wibble
pub fn main() {
  wibble.example_fn()
}
"#;

    assert_signature_help!(
        TestProject::for_source(code)
            .add_module("example", "pub fn example_fn(a: Int, b: String) { Nil }"),
        find_position_of("example_fn()").under_last_char()
    );
}

#[test]
pub fn help_shows_labels() {
    assert_signature_help!(
        r#"
pub fn wibble(a: Int, b b: Int, c c: String) { 1.0 }

pub fn main() {
    wibble()
}
    "#,
        find_position_of("wibble()").under_last_char()
    );
}

#[test]
pub fn help_shows_labelled_argument_after_all_unlabelled() {
    assert_signature_help!(
        r#"
pub fn wibble(a: Int, b b: Int, c c: String) { 1.0 }

pub fn main() {
    wibble(1,)
}
    "#,
        find_position_of("wibble(1,)").under_last_char()
    );
}

#[test]
pub fn help_shows_first_missing_labelled_argument_if_out_of_order() {
    assert_signature_help!(
        r#"
pub fn wibble(a a: Int, b b: Int, c c: String) { 1.0 }

pub fn main() {
    wibble(c: "c",)
}
    "#,
        find_position_of("wibble(c: \"c\",)").under_last_char()
    );
}

#[test]
pub fn help_for_piped_imported_function_starts_from_second_argument() {
    let code = r#"
import example
pub fn main() {
    1 |> example.example_fn()
}
    "#;

    assert_signature_help!(
        TestProject::for_source(code)
            .add_module("example", "pub fn example_fn(a: Int, b: String) { Nil }"),
        find_position_of("example_fn()").under_last_char()
    );
}

#[test]
pub fn help_for_piped_function_starts_from_second_argument() {
    assert_signature_help!(
        r#"
pub fn wibble(a a: Int, b b: Int, c c: String) { 1.0 }

pub fn main() {
    1 |> wibble()
}
    "#,
        find_position_of("wibble()").under_last_char()
    );
}

#[test]
pub fn help_for_use_function_call_starts_from_first_argument() {
    assert_signature_help!(
        r#"
pub fn wibble(a: Int, b: Int, c: fn() -> Int) { 1.0 }

pub fn main() {
    use <- wibble()
}
    "#,
        find_position_of("wibble()").under_last_char()
    );
}

#[test]
pub fn help_for_use_function_call_uses_precise_types_when_missing_some_arguments() {
    assert_signature_help!(
        r#"
pub fn guard(a: Bool, b: a, c: fn() -> a) { 1.0 }

pub fn main() {
    use <- guard(True,)
}
    "#,
        find_position_of("guard(True,)").under_last_char()
    );
}

#[test]
pub fn help_for_use_function_shows_next_unlabelled_argument() {
    assert_signature_help!(
        r#"
pub fn guard(a a: Bool, b b: a, c c: fn() -> a) { 1.0 }

pub fn main() {
    use <- guard(b: 1,)
}
    "#,
        find_position_of("guard(b: 1,)").under_last_char()
    );
}

#[test]
pub fn help_does_not_come_up_for_function_that_does_not_exist() {
    assert_no_signature_help!(
        r#"
pub fn main() {
    use <- to_be_or_not_to_be()
}
    "#,
        find_position_of("to_be_or_not_to_be()").under_last_char()
    );
}

#[test]
// Regression introduced by 4112682cdb5d5b0bb6d1defc6cde849b6a6f65ab.
pub fn help_with_labelled_constructor() {
    assert_signature_help!(
        r#"
pub type Pokemon {
    Pokemon(name: String, types: List(String), moves: List(String))
}

pub fn main() {
    Pokemon(name: "Jirachi",)
}
    "#,
        find_position_of(r#"Pokemon(name: "Jirachi",)"#).under_last_char()
    );
}

#[test]
pub fn help_for_use_function_call_uses_generic_names_when_missing_all_arguments() {
    assert_signature_help!(
        r#"
pub fn wibble(x: something, y: fn() -> something, z: anything) { Nil }
pub fn main() {
    wibble( )
}
"#,
        find_position_of("wibble( ").under_last_char()
    );
}

#[test]
pub fn help_for_use_function_call_uses_concrete_types_when_possible_or_generic_names_when_unbound()
{
    assert_signature_help!(
        r#"
pub fn wibble(x: something, y: fn() -> something, z: anything) { Nil }
pub fn main() {
    wibble(1, )
}
"#,
        find_position_of("wibble(1, )").under_last_char()
    );
}

#[test]
pub fn help_for_use_function_call_uses_concrete_types_when_late_ubound() {
    assert_signature_help!(
        r#"
fn identity(x: a) -> a { x }

fn main() {
  let a = Ok(10)
  identity( a)
}
"#,
        find_position_of("identity( ").under_last_char()
    );
}
