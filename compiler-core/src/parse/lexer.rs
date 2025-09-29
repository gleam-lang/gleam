use ecow::EcoString;

use crate::ast::SrcSpan;
use crate::parse::LiteralFloatValue;
use crate::parse::error::{LexicalError, LexicalErrorType};
use crate::parse::token::Token;
use std::char;

use super::error::InvalidUnicodeEscapeError;

#[derive(Debug)]
pub struct Lexer<T: Iterator<Item = (u32, char)>> {
    chars: T,
    pending: Vec<Spanned>,
    chr0: Option<char>,
    chr1: Option<char>,
    loc0: u32,
    loc1: u32,
}
pub type Spanned = (u32, Token, u32);
pub type LexResult = Result<Spanned, LexicalError>;

pub fn str_to_keyword(word: &str) -> Option<Token> {
    // Alphabetical keywords:
    match word {
        "as" => Some(Token::As),
        "assert" => Some(Token::Assert),
        "auto" => Some(Token::Auto),
        "case" => Some(Token::Case),
        "const" => Some(Token::Const),
        "delegate" => Some(Token::Delegate),
        "derive" => Some(Token::Derive),
        "echo" => Some(Token::Echo),
        "else" => Some(Token::Else),
        "fn" => Some(Token::Fn),
        "if" => Some(Token::If),
        "implement" => Some(Token::Implement),
        "import" => Some(Token::Import),
        "let" => Some(Token::Let),
        "macro" => Some(Token::Macro),
        "opaque" => Some(Token::Opaque),
        "panic" => Some(Token::Panic),
        "pub" => Some(Token::Pub),
        "test" => Some(Token::Test),
        "todo" => Some(Token::Todo),
        "type" => Some(Token::Type),
        "use" => Some(Token::Use),
        _ => None,
    }
}

pub fn make_tokenizer(source: &str) -> impl Iterator<Item = LexResult> + '_ {
    let chars = source.char_indices().map(|(i, c)| (i as u32, c));
    let nlh = NewlineHandler::new(chars);
    Lexer::new(nlh)
}

// The newline handler is an iterator which collapses different newline
// types into \n always.
#[derive(Debug)]
pub struct NewlineHandler<T: Iterator<Item = (u32, char)>> {
    source: T,
    chr0: Option<(u32, char)>,
    chr1: Option<(u32, char)>,
}

impl<T> NewlineHandler<T>
where
    T: Iterator<Item = (u32, char)>,
{
    pub fn new(source: T) -> Self {
        let mut nlh = NewlineHandler {
            source,
            chr0: None,
            chr1: None,
        };
        let _ = nlh.shift();
        let _ = nlh.shift();
        nlh
    }

    fn shift(&mut self) -> Option<(u32, char)> {
        let result = self.chr0;
        self.chr0 = self.chr1;
        self.chr1 = self.source.next();
        result
    }
}

impl<T> Iterator for NewlineHandler<T>
where
    T: Iterator<Item = (u32, char)>,
{
    type Item = (u32, char);

    fn next(&mut self) -> Option<Self::Item> {
        // Collapse \r\n into \n
        if let Some((i, '\r')) = self.chr0 {
            if let Some((_, '\n')) = self.chr1 {
                // Transform windows EOL into \n
                let _ = self.shift();
                // using the position from the \r
                self.chr0 = Some((i, '\n'))
            } else {
                // Transform MAC EOL into \n
                self.chr0 = Some((i, '\n'))
            }
        }

        self.shift()
    }
}

impl<T> Lexer<T>
where
    T: Iterator<Item = (u32, char)>,
{
    pub fn new(input: T) -> Self {
        let mut lxr = Lexer {
            chars: input,
            pending: Vec::new(),
            chr0: None,
            chr1: None,
            loc0: 0,
            loc1: 0,
        };
        let _ = lxr.next_char();
        let _ = lxr.next_char();
        lxr
    }

    // This is the main entry point. Call this function to retrieve the next token.
    // This function is used by the iterator implementation.
    fn inner_next(&mut self) -> LexResult {
        // top loop, keep on processing, until we have something pending.
        while self.pending.is_empty() {
            self.consume_normal()?;
        }

        Ok(self.pending.remove(0))
    }

    // Take a look at the next character, if any, and decide upon the next steps.
    fn consume_normal(&mut self) -> Result<(), LexicalError> {
        // Check if we have some character:
        if let Some(c) = self.chr0 {
            let mut check_for_minus = false;
            if self.is_upname_start(c) {
                let name = self.lex_upname()?;
                self.emit(name)
            } else if self.is_name_start(c) {
                check_for_minus = true;
                let name = self.lex_name()?;
                self.emit(name);
            } else if self.is_number_start(c, self.chr1) {
                check_for_minus = true;
                let num = self.lex_number()?;
                self.emit(num);
            } else {
                self.consume_character(c)?;
            }
            if check_for_minus {
                // We want to lex `1-1` and `x-1` as `1 - 1` and `x - 1`
                if Some('-') == self.chr0 && self.is_number_start('-', self.chr1) {
                    self.eat_single_char(Token::Minus);
                }
            }
        } else {
            // We reached end of file.
            let tok_pos = self.get_pos();
            self.emit((tok_pos, Token::EndOfFile, tok_pos));
        }

        Ok(())
    }

    fn consume_character(&mut self, c: char) -> Result<(), LexicalError> {
        match c {
            '@' => {
                self.eat_single_char(Token::At);
            }
            '"' => {
                let string = self.lex_string()?;
                self.emit(string);
            }
            '=' => {
                let tok_start = self.get_pos();
                let _ = self.next_char();
                match self.chr0 {
                    Some('=') => {
                        let _ = self.next_char();
                        let tok_end = self.get_pos();
                        if let Some('=') = self.chr0 {
                            return Err(LexicalError {
                                error: LexicalErrorType::InvalidTripleEqual,
                                location: SrcSpan {
                                    start: tok_start,
                                    end: tok_end + 1,
                                },
                            });
                        };
                        self.emit((tok_start, Token::EqualEqual, tok_end));
                    }
                    _ => {
                        let tok_end = self.get_pos();
                        self.emit((tok_start, Token::Equal, tok_end));
                    }
                }
            }
            '+' => {
                let tok_start = self.get_pos();
                let _ = self.next_char();
                if let Some('.') = self.chr0 {
                    let _ = self.next_char();
                    let tok_end = self.get_pos();
                    self.emit((tok_start, Token::PlusDot, tok_end));
                } else {
                    let tok_end = self.get_pos();
                    self.emit((tok_start, Token::Plus, tok_end));
                }
            }
            '*' => {
                let tok_start = self.get_pos();
                let _ = self.next_char();
                match self.chr0 {
                    Some('.') => {
                        let _ = self.next_char();
                        let tok_end = self.get_pos();
                        self.emit((tok_start, Token::StarDot, tok_end));
                    }
                    _ => {
                        let tok_end = self.get_pos();
                        self.emit((tok_start, Token::Star, tok_end));
                    }
                }
            }
            '/' => {
                let tok_start = self.get_pos();
                let _ = self.next_char();
                match self.chr0 {
                    Some('.') => {
                        let _ = self.next_char();
                        let tok_end = self.get_pos();
                        self.emit((tok_start, Token::SlashDot, tok_end));
                    }
                    Some('/') => {
                        let _ = self.next_char();
                        let comment = self.lex_comment();
                        self.emit(comment);
                    }
                    _ => {
                        let tok_end = self.get_pos();
                        self.emit((tok_start, Token::Slash, tok_end));
                    }
                }
            }
            '%' => {
                self.eat_single_char(Token::Percent);
            }
            '|' => {
                let tok_start = self.get_pos();
                let _ = self.next_char();
                if let Some('|') = self.chr0 {
                    let _ = self.next_char();
                    let tok_end = self.get_pos();
                    self.emit((tok_start, Token::VbarVbar, tok_end));
                } else if let Some('>') = self.chr0 {
                    let _ = self.next_char();
                    let tok_end = self.get_pos();
                    self.emit((tok_start, Token::Pipe, tok_end));
                } else {
                    let tok_end = self.get_pos();
                    self.emit((tok_start, Token::Vbar, tok_end));
                }
            }
            '&' => {
                let tok_start = self.get_pos();
                let _ = self.next_char();
                if let Some('&') = self.chr0 {
                    let _ = self.next_char();
                    let tok_end = self.get_pos();
                    self.emit((tok_start, Token::AmperAmper, tok_end));
                } else {
                    return Err(LexicalError {
                        error: LexicalErrorType::UnrecognizedToken { tok: '&' },
                        location: SrcSpan {
                            start: tok_start,
                            end: tok_start,
                        },
                    });
                }
            }
            '-' => {
                let tok_start = self.get_pos();
                let _ = self.next_char();
                match self.chr0 {
                    Some('.') => {
                        let _ = self.next_char();
                        let tok_end = self.get_pos();
                        self.emit((tok_start, Token::MinusDot, tok_end));
                    }
                    Some('>') => {
                        let _ = self.next_char();
                        let tok_end = self.get_pos();
                        self.emit((tok_start, Token::RArrow, tok_end));
                    }
                    _ => {
                        let tok_end = self.get_pos();
                        self.emit((tok_start, Token::Minus, tok_end));
                    }
                }
            }
            '!' => {
                let tok_start = self.get_pos();
                let _ = self.next_char();
                if let Some('=') = self.chr0 {
                    let _ = self.next_char();
                    let tok_end = self.get_pos();
                    self.emit((tok_start, Token::NotEqual, tok_end));
                } else {
                    let tok_end = self.get_pos();
                    self.emit((tok_start, Token::Bang, tok_end));
                }
            }
            '(' => {
                self.eat_single_char(Token::LeftParen);
            }
            ')' => {
                self.eat_single_char(Token::RightParen);
            }
            '[' => {
                self.eat_single_char(Token::LeftSquare);
            }
            ']' => {
                self.eat_single_char(Token::RightSquare);
            }
            '{' => {
                self.eat_single_char(Token::LeftBrace);
            }
            '}' => {
                self.eat_single_char(Token::RightBrace);
            }
            ':' => {
                self.eat_single_char(Token::Colon);
            }
            '<' => {
                let tok_start = self.get_pos();
                let _ = self.next_char();
                match self.chr0 {
                    Some('>') => {
                        let _ = self.next_char();
                        let tok_end = self.get_pos();
                        self.emit((tok_start, Token::LtGt, tok_end));
                    }
                    Some('<') => {
                        let _ = self.next_char();
                        let tok_end = self.get_pos();
                        self.emit((tok_start, Token::LtLt, tok_end));
                    }
                    Some('.') => {
                        let _ = self.next_char();
                        let tok_end = self.get_pos();
                        self.emit((tok_start, Token::LessDot, tok_end));
                    }
                    Some('-') => {
                        let _ = self.next_char();
                        let tok_end = self.get_pos();
                        self.emit((tok_start, Token::LArrow, tok_end));
                    }
                    Some('=') => {
                        let _ = self.next_char();
                        match self.chr0 {
                            Some('.') => {
                                let _ = self.next_char();
                                let tok_end = self.get_pos();
                                self.emit((tok_start, Token::LessEqualDot, tok_end));
                            }
                            _ => {
                                let tok_end = self.get_pos();
                                self.emit((tok_start, Token::LessEqual, tok_end));
                            }
                        }
                    }
                    _ => {
                        let tok_end = self.get_pos();
                        self.emit((tok_start, Token::Less, tok_end));
                    }
                }
            }
            '>' => {
                let tok_start = self.get_pos();
                let _ = self.next_char();
                match self.chr0 {
                    Some('>') => {
                        let _ = self.next_char();
                        let tok_end = self.get_pos();
                        self.emit((tok_start, Token::GtGt, tok_end));
                    }
                    Some('.') => {
                        let _ = self.next_char();
                        let tok_end = self.get_pos();
                        self.emit((tok_start, Token::GreaterDot, tok_end));
                    }
                    Some('=') => {
                        let _ = self.next_char();
                        match self.chr0 {
                            Some('.') => {
                                let _ = self.next_char();
                                let tok_end = self.get_pos();
                                self.emit((tok_start, Token::GreaterEqualDot, tok_end));
                            }
                            _ => {
                                let tok_end = self.get_pos();
                                self.emit((tok_start, Token::GreaterEqual, tok_end));
                            }
                        }
                    }
                    _ => {
                        let tok_end = self.get_pos();
                        self.emit((tok_start, Token::Greater, tok_end));
                    }
                }
            }
            ',' => {
                self.eat_single_char(Token::Comma);
            }
            '.' => {
                let tok_start = self.get_pos();
                let _ = self.next_char();
                if let Some('.') = &self.chr0 {
                    let _ = self.next_char();
                    let tok_end = self.get_pos();
                    self.emit((tok_start, Token::DotDot, tok_end));
                } else {
                    let tok_end = self.get_pos();
                    self.emit((tok_start, Token::Dot, tok_end));
                    self.maybe_lex_dot_access()?;
                }
            }
            '#' => {
                self.eat_single_char(Token::Hash);
            }
            '\n' | ' ' | '\t' | '\x0C' => {
                let tok_start = self.get_pos();
                let _ = self.next_char();
                let tok_end = self.get_pos();
                if c == '\n' {
                    self.emit((tok_start, Token::NewLine, tok_end));
                }
            }
            c => {
                let location = self.get_pos();
                return Err(LexicalError {
                    error: LexicalErrorType::UnrecognizedToken { tok: c },
                    location: SrcSpan {
                        start: location,
                        end: location,
                    },
                });
            }
        }

        Ok(())
    }

    // Lexer helper functions:
    // this can be either a reserved word, or a name
    fn lex_name(&mut self) -> LexResult {
        let mut name = String::new();
        let start_pos = self.get_pos();

        while self.is_name_continuation() {
            name.push(self.next_char().expect("lex_name continue"))
        }

        let end_pos = self.get_pos();

        match str_to_keyword(&name) {
            Some(tok) => Ok((start_pos, tok, end_pos)),
            _ => {
                if name.starts_with('_') {
                    Ok((start_pos, Token::DiscardName { name: name.into() }, end_pos))
                } else {
                    Ok((start_pos, Token::Name { name: name.into() }, end_pos))
                }
            }
        }
    }
    // A type name or constructor
    fn lex_upname(&mut self) -> LexResult {
        let mut name = String::new();
        let start_pos = self.get_pos();

        while self.is_name_continuation() {
            name.push(self.next_char().expect("lex_upname upname"));
        }

        let end_pos = self.get_pos();

        match str_to_keyword(&name) {
            Some(tok) => Ok((start_pos, tok, end_pos)),
            _ => Ok((start_pos, Token::UpName { name: name.into() }, end_pos)),
        }
    }

    fn lex_number(&mut self) -> LexResult {
        let start_pos = self.get_pos();
        let num = if self.chr0 == Some('0') {
            if self.chr1 == Some('x') || self.chr1 == Some('X') {
                // Hex!
                let _ = self.next_char();
                let _ = self.next_char();
                self.lex_number_radix(start_pos, 16, "0x")?
            } else if self.chr1 == Some('o') || self.chr1 == Some('O') {
                // Octal!
                let _ = self.next_char();
                let _ = self.next_char();
                self.lex_number_radix(start_pos, 8, "0o")?
            } else if self.chr1 == Some('b') || self.chr1 == Some('B') {
                // Binary!
                let _ = self.next_char();
                let _ = self.next_char();
                self.lex_number_radix(start_pos, 2, "0b")?
            } else {
                self.lex_decimal_number()?
            }
        } else {
            self.lex_decimal_number()?
        };

        if Some('_') == self.chr0 {
            let location = self.get_pos();
            Err(LexicalError {
                error: LexicalErrorType::NumTrailingUnderscore,
                location: SrcSpan {
                    start: location,
                    end: location,
                },
            })
        } else {
            Ok(num)
        }
    }

    // Lex a hex/octal/decimal/binary number without a decimal point.
    fn lex_number_radix(&mut self, start_pos: u32, radix: u32, prefix: &str) -> LexResult {
        let num = self.radix_run(radix);
        if num.is_empty() {
            let location = self.get_pos() - 1;
            Err(LexicalError {
                error: LexicalErrorType::RadixIntNoValue,
                location: SrcSpan {
                    start: location,
                    end: location,
                },
            })
        } else if radix < 16 && Lexer::<T>::is_digit_of_radix(self.chr0, 16) {
            let location = self.get_pos();
            Err(LexicalError {
                error: LexicalErrorType::DigitOutOfRadix,
                location: SrcSpan {
                    start: location,
                    end: location,
                },
            })
        } else {
            let value = format!("{prefix}{num}");
            let int_value = super::parse_int_value(&value).expect("int value to parse as bigint");
            let end_pos = self.get_pos();
            Ok((
                start_pos,
                Token::Int {
                    value: value.into(),
                    int_value,
                },
                end_pos,
            ))
        }
    }

    // Lex a normal number, that is, no octal, hex or binary number.
    // This function cannot be reached without the head of the stream being either 0-9 or '-', 0-9
    fn lex_decimal_number(&mut self) -> LexResult {
        self.lex_decimal_or_int_number(true)
    }

    fn lex_int_number(&mut self) -> LexResult {
        self.lex_decimal_or_int_number(false)
    }

    fn lex_decimal_or_int_number(&mut self, can_lex_decimal: bool) -> LexResult {
        let start_pos = self.get_pos();
        let mut value = String::new();
        // consume negative sign
        if self.chr0 == Some('-') {
            value.push(self.next_char().expect("lex_normal_number negative"));
        }
        // consume first run of digits
        value.push_str(&self.radix_run(10));

        // If float:
        if can_lex_decimal && self.chr0 == Some('.') {
            value.push(self.next_char().expect("lex_normal_number float"));
            value.push_str(&self.radix_run(10));

            // If scientific:
            if self.chr0 == Some('e') {
                value.push(self.next_char().expect("lex_normal_number scientific"));
                if self.chr0 == Some('-') {
                    value.push(
                        self.next_char()
                            .expect("lex_normal_number scientific negative"),
                    );
                }
                let exponent_run = self.radix_run(10);
                if exponent_run.is_empty() {
                    return Err(LexicalError {
                        error: LexicalErrorType::MissingExponent,
                        location: SrcSpan::new(start_pos, self.get_pos()),
                    });
                }
                value.push_str(&exponent_run);
            }
            let end_pos = self.get_pos();
            let float_value =
                LiteralFloatValue::parse(&value).expect("float value to parse as non-NaN f64");
            Ok((
                start_pos,
                Token::Float {
                    value: value.into(),
                    float_value,
                },
                end_pos,
            ))
        } else {
            let int_value = super::parse_int_value(&value).expect("int value to parse as bigint");
            let end_pos = self.get_pos();
            Ok((
                start_pos,
                Token::Int {
                    value: value.into(),
                    int_value,
                },
                end_pos,
            ))
        }
    }

    // Maybe lex dot access that comes after name token.
    fn maybe_lex_dot_access(&mut self) -> Result<(), LexicalError> {
        // It can be nested like: `tuple.1.2.3.4`
        loop {
            if matches!(self.chr0, Some('0'..='9')) {
                let number = self.lex_int_number()?;
                self.emit(number);
            } else {
                break;
            }
        }
        Ok(())
    }

    // Consume a sequence of numbers with the given radix,
    // the digits can be decorated with underscores
    // like this: '1_2_3_4' == '1234'
    fn radix_run(&mut self, radix: u32) -> String {
        let mut value_text = String::new();

        loop {
            if let Some(c) = self.take_number(radix) {
                value_text.push(c);
            } else if self.chr0 == Some('_') && Lexer::<T>::is_digit_of_radix(self.chr1, radix) {
                value_text.push('_');
                let _ = self.next_char();
            } else {
                break;
            }
        }
        value_text
    }

    // Consume a single character with the given radix.
    fn take_number(&mut self, radix: u32) -> Option<char> {
        let take_char = Lexer::<T>::is_digit_of_radix(self.chr0, radix);

        if take_char {
            Some(self.next_char().expect("take_number next char"))
        } else {
            None
        }
    }

    // Test if a digit is of a certain radix.
    fn is_digit_of_radix(c: Option<char>, radix: u32) -> bool {
        match radix {
            2 | 8 | 10 | 16 => c.filter(|c| c.is_digit(radix)).is_some(),
            other => panic!("Radix not implemented: {other}"),
        }
    }

    // There are 3 kinds of comments
    // 2 slash, normal
    // 3 slash, document
    // 4 slash, module
    // this function is entered after 2 slashes
    fn lex_comment(&mut self) -> Spanned {
        enum Kind {
            Comment,
            Doc,
            ModuleDoc,
        }
        let kind = match (self.chr0, self.chr1) {
            (Some('/'), Some('/')) => {
                let _ = self.next_char();
                let _ = self.next_char();
                Kind::ModuleDoc
            }
            (Some('/'), _) => {
                let _ = self.next_char();
                Kind::Doc
            }
            _ => Kind::Comment,
        };
        let mut content = EcoString::new();
        let start_pos = self.get_pos();
        while Some('\n') != self.chr0 {
            match self.chr0 {
                Some(c) => content.push(c),
                None => break,
            }
            let _ = self.next_char();
        }
        let end_pos = self.get_pos();
        let token = match kind {
            Kind::Comment => Token::CommentNormal,
            Kind::Doc => Token::CommentDoc { content },
            Kind::ModuleDoc => Token::CommentModule,
        };
        (start_pos, token, end_pos)
    }

    fn lex_string(&mut self) -> LexResult {
        let start_pos = self.get_pos();
        // advance past the first quote
        let _ = self.next_char();
        let mut string_content = String::new();

        loop {
            match self.next_char() {
                Some('\\') => {
                    let slash_pos = self.get_pos() - 1;
                    if let Some(c) = self.chr0 {
                        match c {
                            'f' | 'n' | 'r' | 't' | '"' | '\\' => {
                                let _ = self.next_char();
                                string_content.push('\\');
                                string_content.push(c);
                            }
                            'u' => {
                                let _ = self.next_char();

                                if self.chr0 != Some('{') {
                                    return Err(LexicalError {
                                        error: LexicalErrorType::InvalidUnicodeEscape(
                                            InvalidUnicodeEscapeError::MissingOpeningBrace,
                                        ),
                                        location: SrcSpan {
                                            start: self.get_pos() - 1,
                                            end: self.get_pos(),
                                        },
                                    });
                                }

                                // All digits inside \u{...}.
                                let mut hex_digits = String::new();

                                loop {
                                    let _ = self.next_char();

                                    let Some(chr) = self.chr0 else {
                                        break;
                                    };

                                    // Don't break early when we've reached 6 digits to ensure a
                                    // useful error message
                                    if chr == '}' {
                                        break;
                                    }

                                    hex_digits.push(chr);

                                    if !chr.is_ascii_hexdigit() {
                                        return Err(LexicalError {
                                            error: LexicalErrorType::InvalidUnicodeEscape(
                                                InvalidUnicodeEscapeError::ExpectedHexDigitOrCloseBrace,
                                            ),
                                            location: SrcSpan {
                                                start: self.get_pos(),
                                                end: self.get_pos() + 1,
                                            },
                                        });
                                    }
                                }

                                if self.chr0 != Some('}') {
                                    return Err(LexicalError {
                                        error: LexicalErrorType::InvalidUnicodeEscape(
                                            InvalidUnicodeEscapeError::ExpectedHexDigitOrCloseBrace,
                                        ),
                                        location: SrcSpan {
                                            start: self.get_pos() - 1,
                                            end: self.get_pos(),
                                        },
                                    });
                                }

                                let _ = self.next_char();

                                if !(1..=6).contains(&hex_digits.len()) {
                                    return Err(LexicalError {
                                        error: LexicalErrorType::InvalidUnicodeEscape(
                                            InvalidUnicodeEscapeError::InvalidNumberOfHexDigits,
                                        ),
                                        location: SrcSpan {
                                            start: slash_pos,
                                            end: self.get_pos(),
                                        },
                                    });
                                }

                                // Checks for i >= 0x110000 || (i >= 0xD800 && i < 0xE000),
                                // where i is the unicode codepoint.
                                if char::from_u32(u32::from_str_radix(&hex_digits, 16).expect(
                                    "Cannot parse codepoint number in Unicode escape sequence",
                                ))
                                .is_none()
                                {
                                    return Err(LexicalError {
                                        error: LexicalErrorType::InvalidUnicodeEscape(
                                            InvalidUnicodeEscapeError::InvalidCodepoint,
                                        ),
                                        location: SrcSpan {
                                            start: slash_pos,
                                            end: self.get_pos(),
                                        },
                                    });
                                }

                                string_content.push_str("\\u{");
                                string_content.push_str(&hex_digits);
                                string_content.push('}');
                            }
                            _ => {
                                return Err(LexicalError {
                                    error: LexicalErrorType::BadStringEscape,
                                    location: SrcSpan {
                                        start: slash_pos,
                                        end: slash_pos + 1,
                                    },
                                });
                            }
                        }
                    } else {
                        return Err(LexicalError {
                            error: LexicalErrorType::BadStringEscape,
                            location: SrcSpan {
                                start: slash_pos,
                                end: slash_pos,
                            },
                        });
                    }
                }
                Some('"') => break,
                Some(c) => string_content.push(c),
                None => {
                    return Err(LexicalError {
                        error: LexicalErrorType::UnexpectedStringEnd,
                        location: SrcSpan {
                            start: start_pos,
                            end: start_pos,
                        },
                    });
                }
            }
        }
        let end_pos = self.get_pos();

        let tok = Token::String {
            value: string_content.into(),
        };

        Ok((start_pos, tok, end_pos))
    }

    fn is_name_start(&self, c: char) -> bool {
        matches!(c, '_' | 'a'..='z')
    }
    fn is_upname_start(&self, c: char) -> bool {
        c.is_ascii_uppercase()
    }
    fn is_number_start(&self, c: char, c1: Option<char>) -> bool {
        match c {
            '0'..='9' => true,
            '-' => matches!(c1, Some('0'..='9')),
            _ => false,
        }
    }

    fn is_name_continuation(&self) -> bool {
        self.chr0
            .map(|c| matches!(c, '_' | '0'..='9' | 'a'..='z' | 'A'..='Z'))
            .unwrap_or(false)
    }

    // advance the stream and emit a token
    fn eat_single_char(&mut self, ty: Token) {
        let tok_start = self.get_pos();
        let _ = self.next_char().expect("eat_single_char");
        let tok_end = self.get_pos();
        self.emit((tok_start, ty, tok_end));
    }

    // Helper function to go to the next character coming up.
    fn next_char(&mut self) -> Option<char> {
        let c = self.chr0;
        let nxt = match self.chars.next() {
            Some((loc, c)) => {
                self.loc0 = self.loc1;
                self.loc1 = loc;
                Some(c)
            }
            None => {
                // EOF needs a single advance
                self.loc0 = self.loc1;
                self.loc1 += 1;
                None
            }
        };
        self.chr0 = self.chr1;
        self.chr1 = nxt;
        c
    }

    // Helper function to retrieve the current position.
    fn get_pos(&self) -> u32 {
        self.loc0
    }

    // Helper function to emit a lexed token to the queue of tokens.
    fn emit(&mut self, spanned: Spanned) {
        self.pending.push(spanned);
    }
}

impl<T> Iterator for Lexer<T>
where
    T: Iterator<Item = (u32, char)>,
{
    type Item = LexResult;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.inner_next();

        match token {
            Ok((_, Token::EndOfFile, _)) => None,
            r => Some(r),
        }
    }
}
