use lazy_static::lazy_static;

/// Check if a module name is a valid gleam module name.
pub fn is_gleam_module(module: &str) -> bool {
    use regex::Regex;
    lazy_static! {
        static ref RE: Regex = Regex::new(&format!(
            "^({module}{slash})*{module}$",
            module = "[a-z][_a-z0-9]*",
            slash = "/",
        ))
        .expect("is_gleam_module() RE regex");
    }

    RE.is_match(module)
}

#[test]
fn invalid_module_names() {
    for mod_name in [
        "",
        "/mod/name",
        "/mod/name/",
        "mod/name/",
        "/mod/",
        "mod/",
        "common-invalid-character",
    ] {
        assert!(!is_gleam_module(mod_name));
    }
}

#[test]
fn valid_module_names() {
    for mod_name in ["valid", "valid/name", "valid/mod/name"] {
        assert!(is_gleam_module(mod_name));
    }
}
