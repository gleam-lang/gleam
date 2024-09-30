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
