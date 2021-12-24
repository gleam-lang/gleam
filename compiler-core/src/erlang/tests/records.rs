use crate::erlang::*;
use crate::type_;

#[test]
fn basic() {
    insta::assert_snapshot!(record_definition(
        "PetCat",
        &[
            ("name", type_::tuple(vec![])),
            ("is_cute", type_::tuple(vec![]))
        ]
    ));
}

#[test]
fn reserve_words() {
    // Reserved words are escaped in record names and fields
    insta::assert_snapshot!(record_definition(
        "div",
        &[
            ("receive", type_::int()),
            ("catch", type_::tuple(vec![])),
            ("unreserved", type_::tuple(vec![]))
        ]
    ));
}

#[test]
fn type_vars() {
    // Type vars are printed as `any()` because records don't support generics
    insta::assert_snapshot!(record_definition(
        "PetCat",
        &[
            ("name", type_::generic_var(1)),
            ("is_cute", type_::unbound_var(1)),
            ("linked", type_::link(type_::int()))
        ]
    ));
}

#[test]
fn module_types() {
    // Types are printed with module qualifiers
    let module_name = vec!["name".to_string()];
    insta::assert_snapshot!(record_definition(
        "PetCat",
        &[(
            "name",
            Arc::new(type_::Type::App {
                public: true,
                module: module_name,
                name: "my_type".to_string(),
                args: vec![]
            })
        )]
    ));
}

#[test]
fn long_definition_formatting() {
    // Long definition formatting
    insta::assert_snapshot!(record_definition(
        "PetCat",
        &[
            ("name", type_::generic_var(1)),
            ("is_cute", type_::unbound_var(1)),
            ("linked", type_::link(type_::int())),
            (
                "whatever",
                type_::list(type_::tuple(vec![
                    type_::nil(),
                    type_::list(type_::tuple(vec![type_::nil(), type_::nil(), type_::nil()])),
                    type_::nil(),
                    type_::list(type_::tuple(vec![type_::nil(), type_::nil(), type_::nil()])),
                    type_::nil(),
                    type_::list(type_::tuple(vec![type_::nil(), type_::nil(), type_::nil()])),
                ]))
            ),
        ]
    ));
}
