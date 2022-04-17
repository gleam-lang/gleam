use crate::ast::SrcSpan;
use crate::parse::error::{LexicalError, LexicalErrorType};
use crate::parse::token::Token;
use std::char;

#[derive(Debug)]
pub struct Lexer<T: Iterator<Item = (usize, char)>> {
    chars: T,
    pending: Vec<Spanned>,
    chr0: Option<char>,
    chr1: Option<char>,
    loc0: usize,
    loc1: usize,
    location: usize,
}
pub type Spanned = (usize, Token, usize);
pub type LexResult = Result<Spanned, LexicalError>;

pub fn str_to_keyword(word: &str) -> Option<Token> {
    // Alphabetical keywords:
    match word {
        "as" => Some(Token::As),
        "assert" => Some(Token::Assert),
        "case" => Some(Token::Case),
        "const" => Some(Token::Const),
        "external" => Some(Token::External),
        "fn" => Some(Token::Fn),
        "if" => Some(Token::If),
        "import" => Some(Token::Import),
        "let" => Some(Token::Let),
        "opaque" => Some(Token::Opaque),
        "pub" => Some(Token::Pub),
        "todo" => Some(Token::Todo),
        "try" => Some(Token::Try),
        "type" => Some(Token::Type),
        _ => None,
    }
}

pub fn make_tokenizer(source: &str) -> impl Iterator<Item = LexResult> + '_ {
    let nlh = NewlineHandler::new(source.char_indices());
    Lexer::new(nlh)
}

// The newline handler is an iterator which collapses different newline
// types into \n always.
#[derive(Debug)]
pub struct NewlineHandler<T: Iterator<Item = (usize, char)>> {
    source: T,
    chr0: Option<(usize, char)>,
    chr1: Option<(usize, char)>,
}

impl<T> NewlineHandler<T>
where
    T: Iterator<Item = (usize, char)>,
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

    fn shift(&mut self) -> Option<(usize, char)> {
        let result = self.chr0;
        self.chr0 = self.chr1;
        self.chr1 = self.source.next();
        result
    }
}

impl<T> Iterator for NewlineHandler<T>
where
    T: Iterator<Item = (usize, char)>,
{
    type Item = (usize, char);

    fn next(&mut self) -> Option<Self::Item> {
        // Collapse \r\n into \n
        while let Some((i, '\r')) = self.chr0 {
            if let Some((_, '\n')) = self.chr1 {
                // Transform windows EOL into \n
                let _ = self.shift();
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
    T: Iterator<Item = (usize, char)>,
{
    pub fn new(input: T) -> Self {
        let mut lxr = Lexer {
            chars: input,
            pending: Vec::new(),
            location: 0,
            chr0: None,
            chr1: None,
            loc0: 0,
            loc1: 0,
        };
        let _ = lxr.next_char();
        let _ = lxr.next_char();
        lxr.location = 0;
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
                let tok_start = self.get_pos();
                let _ = self.next_char();
                let tok_end = self.get_pos();
                self.emit((tok_start, Token::Percent, tok_end));
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
                }
            }
            '#' => {
                self.eat_single_char(Token::Hash);
            }
            '\n' => {
                let _ = self.next_char();
                let tok_start = self.get_pos();
                while let Some(c) = self.chr0 {
                    match c {
                        ' ' | '\t' | '\x0C' => {
                            let _ = self.next_char();
                        }
                        '\n' => {
                            let tok_end = self.get_pos();
                            self.emit((tok_start, Token::EmptyLine, tok_end));
                            break;
                        }
                        _ => break,
                    }
                }
            }
            ' ' | '\t' | '\x0C' | ';' => {
                // Skip whitespaces and semicolons
                let _ = self.next_char();
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

        // Finish lexing the name and return an error if an uppercase letter is used
        if self.is_name_error_continuation() {
            while self.is_name_error_continuation() {
                name.push(self.next_char().expect("lex_name error"))
            }
            let end_pos = self.get_pos();
            if name.starts_with('_') {
                return Err(LexicalError {
                    error: LexicalErrorType::BadDiscardName { name },
                    location: SrcSpan {
                        start: start_pos,
                        end: end_pos,
                    },
                });
            } else {
                return Err(LexicalError {
                    error: LexicalErrorType::BadName { name },
                    location: SrcSpan {
                        start: start_pos,
                        end: end_pos,
                    },
                });
            }
        }

        let end_pos = self.get_pos();

        if let Some(tok) = str_to_keyword(&name) {
            Ok((start_pos, tok, end_pos))
        } else if name.starts_with('_') {
            Ok((start_pos, Token::DiscardName { name }, end_pos))
        } else {
            Ok((start_pos, Token::Name { name }, end_pos))
        }
    }
    // A type name or constructor
    fn lex_upname(&mut self) -> LexResult {
        let mut name = String::new();
        let start_pos = self.get_pos();

        while self.is_upname_continuation() {
            name.push(self.next_char().expect("lex_upname upname"));
        }

        // Finish lexing the upname and return an error if an underscore is used
        if self.is_name_error_continuation() {
            while self.is_name_error_continuation() {
                name.push(self.next_char().expect("lex_upname name error"))
            }
            let end_pos = self.get_pos();
            return Err(LexicalError {
                error: LexicalErrorType::BadUpname { name },
                location: SrcSpan {
                    start: start_pos,
                    end: end_pos,
                },
            });
        }

        let end_pos = self.get_pos();

        if let Some(tok) = str_to_keyword(&name) {
            Ok((start_pos, tok, end_pos))
        } else {
            Ok((start_pos, Token::UpName { name }, end_pos))
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
                self.lex_normal_number()
            }
        } else {
            self.lex_normal_number()
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
    fn lex_number_radix(&mut self, start_pos: usize, radix: u32, prefix: &str) -> LexResult {
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
            let value = format!("{}{}", prefix, num);
            let end_pos = self.get_pos();
            Ok((start_pos, Token::Int { value }, end_pos))
        }
    }

    // Lex a normal number, that is, no octal, hex or binary number.
    // This function cannot be reached without the head of the stream being either 0-9 or '-', 0-9
    fn lex_normal_number(&mut self) -> Spanned {
        let start_pos = self.get_pos();
        let mut value = String::new();
        // consume negative sign
        if self.chr0 == Some('-') {
            value.push(self.next_char().expect("lex_normal_number negative"));
        }
        // consume first run of digits
        value.push_str(&self.radix_run(10));

        // If float:
        if self.chr0 == Some('.') {
            value.push(self.next_char().expect("lex_normal_number float"));
            value.push_str(&self.radix_run(10));
            let end_pos = self.get_pos();
            (start_pos, Token::Float { value }, end_pos)
        } else {
            let end_pos = self.get_pos();
            (start_pos, Token::Int { value }, end_pos)
        }
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
            other => panic!("Radix not implemented: {}", other),
        }
    }

    // There are 3 kinds of comments
    // 2 slash, normal
    // 3 slash, document
    // 4 slash, module
    // this function is entered after 2 slashes
    fn lex_comment(&mut self) -> Spanned {
        let kind = match (self.chr0, self.chr1) {
            (Some('/'), Some('/')) => {
                let _ = self.next_char();
                let _ = self.next_char();
                Token::CommentModule
            }
            (Some('/'), _) => {
                let _ = self.next_char();
                Token::CommentDoc
            }
            _ => Token::CommentNormal,
        };
        let start_pos = self.get_pos();
        while Some('\n') != self.chr0 && None != self.chr0 {
            let _ = self.next_char();
        }
        let end_pos = self.get_pos();
        (start_pos, kind, end_pos)
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
                            'e' | 'f' | 'n' | 'r' | 't' | '"' | '\\' => {
                                let _ = self.next_char();
                                string_content.push('\\');
                                string_content.push(c);
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
            value: string_content,
        };

        Ok((start_pos, tok, end_pos))
    }

    fn is_name_start(&self, c: char) -> bool {
        matches!(c, '_' | 'a'..='z')
    }
    fn is_upname_start(&self, c: char) -> bool {
        matches!(c, 'A'..='Z')
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
            .map(|c| matches!(c, '_' | '0'..='9' | 'a'..='z'))
            .unwrap_or(false)
    }

    fn is_upname_continuation(&self) -> bool {
        self.chr0
            .map(|c| matches!(c, '0'..='9' | 'a'..='z' | 'A'..='Z'))
            .unwrap_or(false)
    }

    fn is_name_error_continuation(&self) -> bool {
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
    fn get_pos(&self) -> usize {
        self.loc0
    }

    // Helper function to emit a lexed token to the queue of tokens.
    fn emit(&mut self, spanned: Spanned) {
        self.pending.push(spanned);
    }
}

impl<T> Iterator for Lexer<T>
where
    T: Iterator<Item = (usize, char)>,
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
