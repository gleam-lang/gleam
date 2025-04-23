use ecow::EcoString;
use itertools::Itertools;

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
pub fn string_to_utf16_bytes(string: &str) -> Vec<u8> {
    let mut bytes = Vec::with_capacity(string.len() * 2);

    for character in string.chars() {
        let mut character_buffer = [0, 0];

        _ = character.encode_utf16(&mut character_buffer);

        bytes.extend(character_buffer[0].to_le_bytes());

        if character_buffer[1] != 0 {
            bytes.extend(character_buffer[1].to_le_bytes());
        }
    }

    bytes
}

/// Converts a string into its UTF-32 representation in bytes
pub fn string_to_utf32_bytes(string: &str) -> Vec<u8> {
    let mut bytes = Vec::with_capacity(string.len() * 4);

    for character in string.chars() {
        let u32 = character as u32;
        bytes.extend(u32.to_le_bytes());
    }

    bytes
}
