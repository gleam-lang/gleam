use ecow::EcoString;
use itertools::Itertools;
use std::sync::OnceLock;

use crate::ast::Endianness;

/// Converts any escape sequences from the given string to their correct
/// bytewise UTF-8 representation and returns the resulting string.
pub fn convert_string_escape_chars(str: &EcoString) -> EcoString {
    let mut filtered_str = EcoString::new();
    let mut str_iter = str.chars().peekable();
    loop {
        match str_iter.next() {
            Some('\\') => match str_iter.next() {
                // Check for Unicode escape sequence, e.g. \u{00012FF}
                Some('u') => {
                    if str_iter.peek() != Some(&'{') {
                        // Invalid Unicode escape sequence
                        filtered_str.push('u');
                        continue;
                    }

                    // Consume the left brace after peeking
                    let _ = str_iter.next();

                    let codepoint_str = str_iter
                        .peeking_take_while(char::is_ascii_hexdigit)
                        .collect::<String>();

                    if codepoint_str.is_empty() || str_iter.peek() != Some(&'}') {
                        // Invalid Unicode escape sequence
                        filtered_str.push_str("u{");
                        filtered_str.push_str(&codepoint_str);
                        continue;
                    }

                    let codepoint = u32::from_str_radix(&codepoint_str, 16)
                        .ok()
                        .and_then(char::from_u32);

                    if let Some(codepoint) = codepoint {
                        // Consume the right brace after peeking
                        let _ = str_iter.next();

                        // Consider this codepoint's length instead of
                        // that of the Unicode escape sequence itself
                        filtered_str.push(codepoint);
                    } else {
                        // Invalid Unicode escape sequence
                        // (codepoint value not in base 16 or too large)
                        filtered_str.push_str("u{");
                        filtered_str.push_str(&codepoint_str);
                    }
                }
                Some('n') => filtered_str.push('\n'),
                Some('r') => filtered_str.push('\r'),
                Some('f') => filtered_str.push('\u{C}'),
                Some('t') => filtered_str.push('\t'),
                Some('"') => filtered_str.push('\"'),
                Some('\\') => filtered_str.push('\\'),
                Some(c) => filtered_str.push(c),
                None => break,
            },
            Some(c) => filtered_str.push(c),
            None => break,
        }
    }
    filtered_str
}

pub fn to_snake_case(string: &str) -> EcoString {
    let mut snake_case = EcoString::with_capacity(string.len());
    let mut is_word_boundary = true;

    for char in string.chars() {
        match char {
            '_' | ' ' => {
                is_word_boundary = true;
                continue;
            }
            _ if char.is_uppercase() => {
                is_word_boundary = true;
            }
            _ => {}
        }

        if is_word_boundary {
            // We don't want to push an underscore at the start of the string,
            // even if it starts with a capital letter or other delimiter.
            if !snake_case.is_empty() {
                snake_case.push('_');
            }
            is_word_boundary = false;
        }
        snake_case.push(char.to_ascii_lowercase());
    }

    snake_case
}

pub fn to_upper_camel_case(string: &str) -> EcoString {
    let mut pascal_case = EcoString::with_capacity(string.len());
    let mut chars = string.chars();

    while let Some(char) = chars.next() {
        if char == '_' {
            let Some(next) = chars.next() else { break };
            pascal_case.push(next.to_ascii_uppercase());
        } else {
            pascal_case.push(char);
        }
    }

    pascal_case
}

/// Converts a string into its UTF-16 representation in bytes
pub fn string_to_utf16_bytes(string: &str, endianness: Endianness) -> Vec<u8> {
    let mut bytes = Vec::with_capacity(string.len() * 2);

    let mut character_buffer = [0, 0];
    for character in string.chars() {
        let segments = character.encode_utf16(&mut character_buffer);

        for segment in segments {
            let segment_bytes = match endianness {
                Endianness::Big => segment.to_be_bytes(),
                Endianness::Little => segment.to_le_bytes(),
            };

            bytes.push(segment_bytes[0]);
            bytes.push(segment_bytes[1]);
        }
    }

    bytes
}

/// Converts a string into its UTF-32 representation in bytes
pub fn string_to_utf32_bytes(string: &str, endianness: Endianness) -> Vec<u8> {
    let mut bytes = Vec::with_capacity(string.len() * 4);

    for character in string.chars() {
        let character_bytes = match endianness {
            Endianness::Big => (character as u32).to_be_bytes(),
            Endianness::Little => (character as u32).to_le_bytes(),
        };
        bytes.extend(character_bytes);
    }

    bytes
}

/// Gets the number of UTF-16 codepoints it would take to encode a given string.
pub fn length_utf16(string: &str) -> usize {
    let mut length = 0;

    for char in string.chars() {
        length += char.len_utf16()
    }

    length
}

/// Gets the number of UTF-32 codepoints in a string
pub fn length_utf32(string: &str) -> usize {
    string.chars().count()
}

/// Check if a module name is a valid Gleam module name.
pub fn is_gleam_module(module: &str) -> bool {
    use regex::Regex;
    static RE: OnceLock<Regex> = OnceLock::new();

    RE.get_or_init(|| {
        Regex::new(&format!(
            "^({module}{slash})*{module}$",
            module = "[a-z][_a-z0-9]*",
            slash = "/",
        ))
        .expect("is_gleam_module() regex")
    })
    .is_match(module)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn valid_module_names() {
        // Single segment names
        assert!(is_gleam_module("valid"));
        assert!(is_gleam_module("my_module"));
        assert!(is_gleam_module("module_123"));
        assert!(is_gleam_module("a"));
        assert!(is_gleam_module("z"));
        assert!(is_gleam_module("my_mod_ule"));

        // Multi-segment names
        assert!(is_gleam_module("valid/name"));
        assert!(is_gleam_module("my/nested/module"));
        assert!(is_gleam_module("my_mod/sub_mod"));
        assert!(is_gleam_module("valid/mod/name"));

        // Edge cases
        assert!(is_gleam_module("a/b"));
        assert!(is_gleam_module("mod1/mod2/mod3"));
    }

    #[test]
    fn invalid_module_names() {
        // Empty string
        assert!(!is_gleam_module(""));

        // Starting with uppercase
        assert!(!is_gleam_module("MyModule"));
        assert!(!is_gleam_module("MYMODULE"));

        // Starting with digit
        assert!(!is_gleam_module("123module"));
        assert!(!is_gleam_module("1module"));

        // Invalid characters
        assert!(!is_gleam_module("my-module"));
        assert!(!is_gleam_module("my.module"));
        assert!(!is_gleam_module("my@module"));

        // Trailing slash
        assert!(!is_gleam_module("my/module/"));

        // Leading slash
        assert!(!is_gleam_module("/mod/name"));
        assert!(!is_gleam_module("/mod/name/"));

        // Double slashes
        assert!(!is_gleam_module("my//module"));

        // Slash only
        assert!(!is_gleam_module("/"));
        assert!(!is_gleam_module("//"));

        // Only invalid segments
        assert!(!is_gleam_module("my/MyModule"));
        assert!(!is_gleam_module("MyModule/nested"));
        assert!(!is_gleam_module("valid/123invalid/sub"));
    }
}
