// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2020 The Gleam contributors

use ecow::EcoString;

use crate::parse::LiteralFloatValue;
use crate::parse::error::{LexicalError, LexicalErrorType};
use crate::parse::token::Token;
use src_span::SrcSpan;
use std::char;
use std::ops::Neg;

use super::error::InvalidUnicodeEscapeError;

#[derive(Debug)]
pub struct Lexer<T: Iterator<Item = (u32, char)>> {
    chars: T,
    pending: Vec<Spanned>,
    char0: Option<char>,
    char1: Option<char>,
    location0: u32,
    location1: u32,
}
pub type Spanned = (u32, Token, u32);
pub type LexResult = Result<Spanned, LexicalError>;

pub fn string_to_keyword(word: &str) -> Option<Token> {
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
    let chars = source
        .char_indices()
        .map(|(index, char)| (index as u32, char));
    let new_line_handler = NewlineHandler::new(chars);
    Lexer::new(new_line_handler)
}

// The newline handler is an iterator which collapses different newline
// types into \n always.
#[derive(Debug)]
pub struct NewlineHandler<T: Iterator<Item = (u32, char)>> {
    source: T,
    char0: Option<(u32, char)>,
    char1: Option<(u32, char)>,
}

impl<T> NewlineHandler<T>
where
    T: Iterator<Item = (u32, char)>,
{
    pub fn new(source: T) -> Self {
        let mut new_line_handler = NewlineHandler {
            source,
            char0: None,
            char1: None,
        };
        let _ = new_line_handler.shift();
        let _ = new_line_handler.shift();
        new_line_handler
    }

    fn shift(&mut self) -> Option<(u32, char)> {
        let result = self.char0;
        self.char0 = self.char1;
        self.char1 = self.source.next();
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
        if let Some((index, '\r')) = self.char0 {
            if let Some((_, '\n')) = self.char1 {
                // Transform windows EOL into \n
                let _ = self.shift();
                // using the position from the \r
                self.char0 = Some((index, '\n'));
            } else {
                // Transform MAC EOL into \n
                self.char0 = Some((index, '\n'));
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
        let mut lexer = Lexer {
            chars: input,
            pending: Vec::new(),
            char0: None,
            char1: None,
            location0: 0,
            location1: 0,
        };
        let _ = lexer.next_char();
        let _ = lexer.next_char();

        // Check whether the first character is a UTF-8 byte order mark, and if so, consume it.
        if lexer.char0 == Some('\u{feff}') {
            let _ = lexer.next_char();
        }

        lexer
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
        if let Some(character) = self.char0 {
            let mut check_for_minus = false;
            if self.is_upname_start(character) {
                let name = self.lex_upname()?;
                self.emit(name);
            } else if self.is_name_start(character) {
                check_for_minus = true;
                let name = self.lex_name()?;
                self.emit(name);
            } else if self.is_number_start(character, self.char1) {
                check_for_minus = true;
                let num = self.lex_number()?;
                self.emit(num);
            } else {
                self.consume_character(character)?;
            }
            if check_for_minus {
                // We want to lex `1-1` and `x-1` as `1 - 1` and `x - 1`
                if Some('-') == self.char0 && self.is_number_start('-', self.char1) {
                    self.eat_single_char(Token::Minus);
                }
            }
        } else {
            // We reached end of file.
            let token_pos = self.get_position();
            self.emit((token_pos, Token::EndOfFile, token_pos));
        }

        Ok(())
    }

    fn consume_character(&mut self, character: char) -> Result<(), LexicalError> {
        match character {
            '@' => {
                self.eat_single_char(Token::At);
            }
            '"' => {
                let string = self.lex_string()?;
                self.emit(string);
            }
            '=' => {
                let mut token_start = self.get_position();
                let _ = self.next_char();

                if self.char0 != Some('=') {
                    let token_end = self.get_position();
                    self.emit((token_start, Token::Equal, token_end));
                    return Ok(());
                }

                let _ = self.next_char();

                if self.char0 != Some('=') {
                    self.emit((token_start, Token::EqualEqual, self.get_position()));
                    return Ok(());
                }

                let _ = self.next_char();
                let mut seen_equals = 3;

                if self.char0 != Some('=') {
                    return Err(LexicalError {
                        error: LexicalErrorType::InvalidTripleEqual,
                        location: SrcSpan {
                            start: token_start,
                            end: self.get_position(),
                        },
                    });
                }

                loop {
                    if seen_equals >= 7 {
                        return Err(LexicalError {
                            error: LexicalErrorType::MergeConflictIndicator,
                            location: SrcSpan {
                                start: token_start,
                                end: self.get_position(),
                            },
                        });
                    }

                    if self.char0 == Some('=') {
                        let _ = self.next_char();
                        seen_equals += 1;
                    } else {
                        while seen_equals > 1 {
                            self.emit((token_start, Token::EqualEqual, token_start + 2));
                            token_start += 2;
                            seen_equals -= 2;
                        }
                        if seen_equals > 0 {
                            self.emit((token_start, Token::Equal, self.get_position()));
                        }
                        break;
                    }
                }
            }
            '+' => {
                let token_start = self.get_position();
                let _ = self.next_char();
                match self.char0 {
                    Some('.') => {
                        let _ = self.next_char();
                        let token_end = self.get_position();
                        self.emit((token_start, Token::PlusDot, token_end));
                    }
                    Some('=' | '+') => {
                        let _ = self.next_char();
                        let token_end = self.get_position();
                        return Err(LexicalError {
                            error: LexicalErrorType::UnsupportedProceduralOperator,
                            location: SrcSpan {
                                start: token_start,
                                end: token_end,
                            },
                        });
                    }
                    _ => {
                        let token_end = self.get_position();
                        self.emit((token_start, Token::Plus, token_end));
                    }
                }
            }
            '*' => {
                let token_start = self.get_position();
                let _ = self.next_char();
                match self.char0 {
                    Some('.') => {
                        let _ = self.next_char();
                        let token_end = self.get_position();
                        self.emit((token_start, Token::StarDot, token_end));
                    }
                    Some('=') => {
                        let _ = self.next_char();
                        let token_end = self.get_position();
                        return Err(LexicalError {
                            error: LexicalErrorType::UnsupportedProceduralOperator,
                            location: SrcSpan {
                                start: token_start,
                                end: token_end,
                            },
                        });
                    }
                    _ => {
                        let token_end = self.get_position();
                        self.emit((token_start, Token::Star, token_end));
                    }
                }
            }
            '/' => {
                let token_start = self.get_position();
                let _ = self.next_char();
                match self.char0 {
                    Some('.') => {
                        let _ = self.next_char();
                        let token_end = self.get_position();
                        self.emit((token_start, Token::SlashDot, token_end));
                    }
                    Some('/') => {
                        let _ = self.next_char();
                        let comment = self.lex_comment();
                        self.emit(comment);
                    }
                    Some('=') => {
                        let _ = self.next_char();
                        let token_end = self.get_position();
                        return Err(LexicalError {
                            error: LexicalErrorType::UnsupportedProceduralOperator,
                            location: SrcSpan {
                                start: token_start,
                                end: token_end,
                            },
                        });
                    }
                    _ => {
                        let token_end = self.get_position();
                        self.emit((token_start, Token::Slash, token_end));
                    }
                }
            }
            '%' => {
                let token_start = self.get_position();
                let _ = self.next_char();
                match self.char0 {
                    Some('=') => {
                        let _ = self.next_char();
                        let token_end = self.get_position();
                        return Err(LexicalError {
                            error: LexicalErrorType::UnsupportedProceduralOperator,
                            location: SrcSpan {
                                start: token_start,
                                end: token_end,
                            },
                        });
                    }
                    _ => {
                        let token_end = self.get_position();
                        self.emit((token_start, Token::Percent, token_end));
                    }
                }
            }
            '|' => {
                let token_start = self.get_position();
                let _ = self.next_char();
                match self.char0 {
                    Some('|') => {
                        let _ = self.next_char();
                        let token_end = self.get_position();
                        self.emit((token_start, Token::VbarVbar, token_end));
                    }
                    Some('>') => {
                        let _ = self.next_char();
                        let token_end = self.get_position();
                        self.emit((token_start, Token::Pipe, token_end));
                    }
                    _ => {
                        let token_end = self.get_position();
                        self.emit((token_start, Token::Vbar, token_end));
                    }
                }
            }
            '&' => {
                let token_start = self.get_position();
                let _ = self.next_char();
                match self.char0 {
                    Some('&') => {
                        let _ = self.next_char();
                        let token_end = self.get_position();
                        self.emit((token_start, Token::AmperAmper, token_end));
                    }
                    _ => {
                        return Err(LexicalError {
                            error: LexicalErrorType::UnrecognizedToken { token: '&' },
                            location: SrcSpan {
                                start: token_start,
                                end: token_start,
                            },
                        });
                    }
                }
            }
            '-' => {
                let token_start = self.get_position();
                let _ = self.next_char();
                match self.char0 {
                    Some('.') => {
                        let _ = self.next_char();
                        let token_end = self.get_position();
                        self.emit((token_start, Token::MinusDot, token_end));
                    }
                    Some('>') => {
                        let _ = self.next_char();
                        let token_end = self.get_position();
                        self.emit((token_start, Token::RArrow, token_end));
                    }
                    // Not including `-` here because repeated int negation is
                    // valid gleam
                    Some('=') => {
                        let _ = self.next_char();
                        let token_end = self.get_position();
                        return Err(LexicalError {
                            error: LexicalErrorType::UnsupportedProceduralOperator,
                            location: SrcSpan {
                                start: token_start,
                                end: token_end,
                            },
                        });
                    }
                    _ => {
                        let token_end = self.get_position();
                        self.emit((token_start, Token::Minus, token_end));
                    }
                }
            }
            '!' => {
                let token_start = self.get_position();
                let _ = self.next_char();
                if let Some('=') = self.char0 {
                    let _ = self.next_char();
                    let token_end = self.get_position();
                    self.emit((token_start, Token::NotEqual, token_end));
                } else {
                    let token_end = self.get_position();
                    self.emit((token_start, Token::Bang, token_end));
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
                let token_start = self.get_position();
                let _ = self.next_char();
                self.lex_lt(token_start)?;
            }
            '>' => {
                let token_start = self.get_position();
                let _ = self.next_char();
                self.lex_gt(token_start)?;
            }
            ',' => {
                self.eat_single_char(Token::Comma);
            }
            '.' => {
                let token_start = self.get_position();
                let _ = self.next_char();
                if let Some('.') = &self.char0 {
                    let _ = self.next_char();
                    let token_end = self.get_position();
                    self.emit((token_start, Token::DotDot, token_end));
                } else {
                    let token_end = self.get_position();
                    self.emit((token_start, Token::Dot, token_end));
                    self.maybe_lex_dot_access()?;
                }
            }
            '#' => {
                self.eat_single_char(Token::Hash);
            }
            '\n' | ' ' | '\t' | '\x0C' => {
                let token_start = self.get_position();
                let _ = self.next_char();
                let token_end = self.get_position();
                if character == '\n' {
                    self.emit((token_start, Token::NewLine, token_end));
                }
            }
            '\u{201A}' => {
                let location = self.get_position();
                return Err(LexicalError {
                    error: LexicalErrorType::VisuallySimilarInvalidCharacter {
                        name: "low single comma quotation mark",
                        correct: "comma",
                    },
                    location: SrcSpan {
                        start: location,
                        end: location,
                    },
                });
            }
            '\u{FF3B}' => {
                let location = self.get_position();
                return Err(LexicalError {
                    error: LexicalErrorType::VisuallySimilarInvalidCharacter {
                        name: "fullwidth left square bracket",
                        correct: "left square bracket",
                    },
                    location: SrcSpan {
                        start: location,
                        end: location,
                    },
                });
            }
            '\u{FF3D}' => {
                let location = self.get_position();
                return Err(LexicalError {
                    error: LexicalErrorType::VisuallySimilarInvalidCharacter {
                        name: "fullwidth right square bracket",
                        correct: "right square bracket",
                    },
                    location: SrcSpan {
                        start: location,
                        end: location,
                    },
                });
            }
            '\u{FF08}' => {
                let location = self.get_position();
                return Err(LexicalError {
                    error: LexicalErrorType::VisuallySimilarInvalidCharacter {
                        name: "fullwidth left parenthesis",
                        correct: "left parenthesis",
                    },
                    location: SrcSpan {
                        start: location,
                        end: location,
                    },
                });
            }
            '\u{FF09}' => {
                let location = self.get_position();
                return Err(LexicalError {
                    error: LexicalErrorType::VisuallySimilarInvalidCharacter {
                        name: "fullwidth right parenthesis",
                        correct: "right parenthesis",
                    },
                    location: SrcSpan {
                        start: location,
                        end: location,
                    },
                });
            }
            '\u{FF0E}' => {
                let location = self.get_position();
                return Err(LexicalError {
                    error: LexicalErrorType::VisuallySimilarInvalidCharacter {
                        name: "fullwidth full stop",
                        correct: "dot",
                    },
                    location: SrcSpan {
                        start: location,
                        end: location,
                    },
                });
            }
            '\u{3002}' => {
                let location = self.get_position();
                return Err(LexicalError {
                    error: LexicalErrorType::VisuallySimilarInvalidCharacter {
                        name: "ideographic full stop",
                        correct: "dot",
                    },
                    location: SrcSpan {
                        start: location,
                        end: location,
                    },
                });
            }
            '\u{FF1C}' => {
                let location = self.get_position();
                return Err(LexicalError {
                    error: LexicalErrorType::VisuallySimilarInvalidCharacter {
                        name: "fullwidth less-than sign",
                        correct: "less-than sign",
                    },
                    location: SrcSpan {
                        start: location,
                        end: location,
                    },
                });
            }
            '\u{FF1E}' => {
                let location = self.get_position();
                return Err(LexicalError {
                    error: LexicalErrorType::VisuallySimilarInvalidCharacter {
                        name: "fullwidth greater-than sign",
                        correct: "greater-than sign",
                    },
                    location: SrcSpan {
                        start: location,
                        end: location,
                    },
                });
            }
            '\u{FF5C}' => {
                let location = self.get_position();
                return Err(LexicalError {
                    error: LexicalErrorType::VisuallySimilarInvalidCharacter {
                        name: "fullwidth vertical line",
                        correct: "pipe",
                    },
                    location: SrcSpan {
                        start: location,
                        end: location,
                    },
                });
            }
            '\u{FF20}' => {
                let location = self.get_position();
                return Err(LexicalError {
                    error: LexicalErrorType::VisuallySimilarInvalidCharacter {
                        name: "fullwidth commercial at",
                        correct: "at sign",
                    },
                    location: SrcSpan {
                        start: location,
                        end: location,
                    },
                });
            }
            '\u{FF3E}' => {
                let location = self.get_position();
                return Err(LexicalError {
                    error: LexicalErrorType::VisuallySimilarInvalidCharacter {
                        name: "fullwidth circumflex accent",
                        correct: "caret",
                    },
                    location: SrcSpan {
                        start: location,
                        end: location,
                    },
                });
            }
            '\u{FF1A}' => {
                let location = self.get_position();
                return Err(LexicalError {
                    error: LexicalErrorType::VisuallySimilarInvalidCharacter {
                        name: "fullwidth colon",
                        correct: "colon",
                    },
                    location: SrcSpan {
                        start: location,
                        end: location,
                    },
                });
            }
            // Visually similar characters that are not valid Gleam source.
            '\u{201C}' | '\u{201D}' => {
                let location = self.get_position();
                return Err(LexicalError {
                    error: LexicalErrorType::VisuallySimilarInvalidCharacter {
                        name: "double quotation mark",
                        correct: "double quote",
                    },
                    location: SrcSpan {
                        start: location,
                        end: location,
                    },
                });
            }
            '\u{2018}' | '\u{2019}' => {
                let location = self.get_position();
                return Err(LexicalError {
                    error: LexicalErrorType::VisuallySimilarInvalidCharacter {
                        name: "single quotation mark",
                        correct: "single quote",
                    },
                    location: SrcSpan {
                        start: location,
                        end: location,
                    },
                });
            }
            '\u{2013}' => {
                let location = self.get_position();
                return Err(LexicalError {
                    error: LexicalErrorType::VisuallySimilarInvalidCharacter {
                        name: "en dash",
                        correct: "minus sign",
                    },
                    location: SrcSpan {
                        start: location,
                        end: location,
                    },
                });
            }
            '\u{2014}' => {
                let location = self.get_position();
                return Err(LexicalError {
                    error: LexicalErrorType::VisuallySimilarInvalidCharacter {
                        name: "em dash",
                        correct: "minus sign",
                    },
                    location: SrcSpan {
                        start: location,
                        end: location,
                    },
                });
            }
            '\u{2217}' => {
                let location = self.get_position();
                return Err(LexicalError {
                    error: LexicalErrorType::VisuallySimilarInvalidCharacter {
                        name: "asterisk operator",
                        correct: "asterisk",
                    },
                    location: SrcSpan {
                        start: location,
                        end: location,
                    },
                });
            }
            '\u{2215}' => {
                let location = self.get_position();
                return Err(LexicalError {
                    error: LexicalErrorType::VisuallySimilarInvalidCharacter {
                        name: "division slash",
                        correct: "forward slash",
                    },
                    location: SrcSpan {
                        start: location,
                        end: location,
                    },
                });
            }
            '\u{00A0}' => {
                let location = self.get_position();
                return Err(LexicalError {
                    error: LexicalErrorType::VisuallySimilarInvalidCharacter {
                        name: "non-breaking space",
                        correct: "space",
                    },
                    location: SrcSpan {
                        start: location,
                        end: location,
                    },
                });
            }
            '\u{200B}' => {
                let location = self.get_position();
                return Err(LexicalError {
                    error: LexicalErrorType::VisuallySimilarInvalidCharacter {
                        name: "zero-width space",
                        correct: "space",
                    },
                    location: SrcSpan {
                        start: location,
                        end: location,
                    },
                });
            }
            '\u{0430}' => {
                let location = self.get_position();
                return Err(LexicalError {
                    error: LexicalErrorType::VisuallySimilarInvalidCharacter {
                        name: "Cyrillic letter а",
                        correct: "latin letter a",
                    },
                    location: SrcSpan {
                        start: location,
                        end: location,
                    },
                });
            }
            '\u{0435}' => {
                let location = self.get_position();
                return Err(LexicalError {
                    error: LexicalErrorType::VisuallySimilarInvalidCharacter {
                        name: "Cyrillic letter е",
                        correct: "latin letter e",
                    },
                    location: SrcSpan {
                        start: location,
                        end: location,
                    },
                });
            }
            '\u{043E}' => {
                let location = self.get_position();
                return Err(LexicalError {
                    error: LexicalErrorType::VisuallySimilarInvalidCharacter {
                        name: "Cyrillic letter о",
                        correct: "latin letter o",
                    },
                    location: SrcSpan {
                        start: location,
                        end: location,
                    },
                });
            }
            '\u{0440}' => {
                let location = self.get_position();
                return Err(LexicalError {
                    error: LexicalErrorType::VisuallySimilarInvalidCharacter {
                        name: "Cyrillic letter р",
                        correct: "latin letter p",
                    },
                    location: SrcSpan {
                        start: location,
                        end: location,
                    },
                });
            }
            '\u{1D35}' => {
                let location = self.get_position();
                return Err(LexicalError {
                    error: LexicalErrorType::VisuallySimilarInvalidCharacter {
                        name: "modifier letter capital I",
                        correct: "latin letter I",
                    },
                    location: SrcSpan {
                        start: location,
                        end: location,
                    },
                });
            }
            '\u{FF0C}' => {
                let location = self.get_position();
                return Err(LexicalError {
                    error: LexicalErrorType::VisuallySimilarInvalidCharacter {
                        name: "fullwidth comma",
                        correct: "comma",
                    },
                    location: SrcSpan {
                        start: location,
                        end: location,
                    },
                });
            }
            '\u{3001}' => {
                let location = self.get_position();
                return Err(LexicalError {
                    error: LexicalErrorType::VisuallySimilarInvalidCharacter {
                        name: "ideographic comma",
                        correct: "comma",
                    },
                    location: SrcSpan {
                        start: location,
                        end: location,
                    },
                });
            }
            character => {
                let location = self.get_position();
                return Err(LexicalError {
                    error: LexicalErrorType::UnrecognizedToken { token: character },
                    location: SrcSpan {
                        start: location,
                        end: location,
                    },
                });
            }
        }

        Ok(())
    }

    // Once we've found a `<`, proceed from there
    fn lex_lt(&mut self, mut token_start: u32) -> Result<(), LexicalError> {
        match self.char0 {
            Some('>') => {
                let _ = self.next_char();
                self.emit((token_start, Token::Concatenate, self.get_position()));
            }
            Some('<') => {
                let _ = self.next_char();

                if self.char0 != Some('<') {
                    self.emit((token_start, Token::LtLt, self.get_position()));
                    return Ok(());
                }

                let _ = self.next_char();
                let mut seen_lt = 3;

                loop {
                    if seen_lt >= 7 {
                        return Err(LexicalError {
                            error: LexicalErrorType::MergeConflictIndicator,
                            location: SrcSpan {
                                start: token_start,
                                end: self.get_position(),
                            },
                        });
                    }

                    if self.char0 == Some('<') {
                        let _ = self.next_char();
                        seen_lt += 1;
                    } else {
                        while seen_lt > 1 {
                            self.emit((token_start, Token::LtLt, token_start + 2));
                            token_start += 2;
                            seen_lt -= 2;
                        }
                        if seen_lt > 0 {
                            return self.lex_lt(token_start);
                        } else {
                            return Ok(());
                        }
                    }
                }
            }
            Some('.') => {
                let _ = self.next_char();
                self.emit((token_start, Token::LessDot, self.get_position()));
            }
            Some('-') => {
                let _ = self.next_char();
                self.emit((token_start, Token::LArrow, self.get_position()));
            }
            Some('=') => {
                let _ = self.next_char();
                match self.char0 {
                    Some('.') => {
                        let _ = self.next_char();
                        self.emit((token_start, Token::LessEqualDot, self.get_position()));
                    }
                    _ => {
                        self.emit((token_start, Token::LessEqual, self.get_position()));
                    }
                }
            }
            _ => {
                self.emit((token_start, Token::Less, self.get_position()));
            }
        }
        Ok(())
    }

    // Once we've found a `>`
    fn lex_gt(&mut self, mut token_start: u32) -> Result<(), LexicalError> {
        match self.char0 {
            Some('>') => {
                let _ = self.next_char();
                let token_end = self.get_position();

                if self.char0 != Some('>') {
                    self.emit((token_start, Token::GtGt, token_end));
                    return Ok(());
                }

                let _ = self.next_char();
                let mut seen_gt = 3;

                loop {
                    if seen_gt >= 7 {
                        return Err(LexicalError {
                            error: LexicalErrorType::MergeConflictIndicator,
                            location: SrcSpan {
                                start: token_start,
                                end: self.get_position(),
                            },
                        });
                    }

                    if self.char0 == Some('>') {
                        let _ = self.next_char();
                        seen_gt += 1;
                    } else {
                        while seen_gt > 1 {
                            self.emit((token_start, Token::GtGt, token_start + 2));
                            token_start += 2;
                            seen_gt -= 2;
                        }
                        if seen_gt > 0 {
                            return self.lex_gt(token_start);
                        } else {
                            return Ok(());
                        }
                    }
                }
            }
            Some('.') => {
                let _ = self.next_char();
                self.emit((token_start, Token::GreaterDot, self.get_position()));
            }
            Some('=') => {
                let _ = self.next_char();
                match self.char0 {
                    Some('.') => {
                        let _ = self.next_char();
                        self.emit((token_start, Token::GreaterEqualDot, self.get_position()));
                    }
                    _ => {
                        self.emit((token_start, Token::GreaterEqual, self.get_position()));
                    }
                }
            }
            _ => {
                self.emit((token_start, Token::Greater, self.get_position()));
            }
        }
        Ok(())
    }
    // Lexer helper functions:
    // this can be either a reserved word, or a name
    fn lex_name(&mut self) -> LexResult {
        let mut name = String::new();
        let start_position = self.get_position();

        while self.is_name_continuation() {
            name.push(self.next_char().expect("lex_name continue"));
        }

        let end_position = self.get_position();

        match string_to_keyword(&name) {
            Some(token) => Ok((start_position, token, end_position)),
            _ => {
                if name.starts_with('_') {
                    Ok((
                        start_position,
                        Token::DiscardName { name: name.into() },
                        end_position,
                    ))
                } else {
                    Ok((
                        start_position,
                        Token::Name { name: name.into() },
                        end_position,
                    ))
                }
            }
        }
    }
    // A type name or constructor
    fn lex_upname(&mut self) -> LexResult {
        let mut name = String::new();
        let start_position = self.get_position();

        while self.is_name_continuation() {
            name.push(self.next_char().expect("lex_upname upname"));
        }

        let end_position = self.get_position();

        match string_to_keyword(&name) {
            Some(token) => Ok((start_position, token, end_position)),
            _ => Ok((
                start_position,
                Token::UpName { name: name.into() },
                end_position,
            )),
        }
    }

    fn lex_number(&mut self) -> LexResult {
        let start_position = self.get_position();

        // We call this function after making sure that what comes next starts
        // with what seems to be a valid number. If we see that it starts with
        // `-` we consume the token and record that the number is negative.
        let is_negative = if self.char0 == Some('-') {
            let _ = self.next_char();
            true
        } else {
            false
        };

        let number = if self.char0 == Some('0') {
            match self.char1 {
                Some('x' | 'X') => {
                    // Hex!
                    let _ = self.next_char();
                    let _ = self.next_char();
                    self.lex_number_radix(start_position, 16, is_negative, "0x")?
                }
                Some('o' | 'O') => {
                    // Octal!
                    let _ = self.next_char();
                    let _ = self.next_char();
                    self.lex_number_radix(start_position, 8, is_negative, "0o")?
                }
                Some('b' | 'B') => {
                    // Binary!
                    let _ = self.next_char();
                    let _ = self.next_char();
                    self.lex_number_radix(start_position, 2, is_negative, "0b")?
                }
                _ => self.lex_decimal_number(start_position, is_negative)?,
            }
        } else {
            self.lex_decimal_number(start_position, is_negative)?
        };

        if Some('_') == self.char0 {
            let location = self.get_position();
            Err(LexicalError {
                error: LexicalErrorType::NumTrailingUnderscore,
                location: SrcSpan {
                    start: location,
                    end: location,
                },
            })
        } else {
            Ok(number)
        }
    }

    // Lex a hex/octal/decimal/binary number without a decimal point.
    fn lex_number_radix(
        &mut self,
        start_pos: u32,
        radix: u32,
        is_negative: bool,
        prefix: &str,
    ) -> LexResult {
        let num = self.radix_run(radix);
        if num.is_empty() {
            let location = self.get_position() - 1;
            Err(LexicalError {
                error: LexicalErrorType::RadixIntNoValue,
                location: SrcSpan {
                    start: location,
                    end: location,
                },
            })
        } else if radix < 16 && Lexer::<T>::is_digit_of_radix(self.char0, 16) {
            let location = self.get_position();
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
            let end_pos = self.get_position();

            let (value, int_value) = if is_negative {
                (format!("-{value}"), int_value.neg())
            } else {
                (value, int_value)
            };

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
    fn lex_decimal_number(&mut self, start_pos: u32, is_negative: bool) -> LexResult {
        self.lex_decimal_or_int_number(start_pos, is_negative, true)
    }

    fn lex_int_number(&mut self, start_pos: u32, is_negative: bool) -> LexResult {
        self.lex_decimal_or_int_number(start_pos, is_negative, false)
    }

    fn lex_decimal_or_int_number(
        &mut self,
        start_pos: u32,
        is_negative: bool,
        can_lex_decimal: bool,
    ) -> LexResult {
        let mut value = String::new();
        if is_negative {
            value.push('-')
        };
        // consume first run of digits
        value.push_str(&self.radix_run(10));

        // If float:
        if can_lex_decimal && self.char0 == Some('.') {
            value.push(self.next_char().expect("lex_normal_number float"));
            value.push_str(&self.radix_run(10));

            // If scientific:
            if self.char0 == Some('e') {
                value.push(self.next_char().expect("lex_normal_number scientific"));
                if self.char0 == Some('-') {
                    value.push(
                        self.next_char()
                            .expect("lex_normal_number scientific negative"),
                    );
                }
                let exponent_run = self.radix_run(10);
                if exponent_run.is_empty() {
                    return Err(LexicalError {
                        error: LexicalErrorType::MissingExponent,
                        location: SrcSpan::new(start_pos, self.get_position()),
                    });
                }
                value.push_str(&exponent_run);
            }
            let end_pos = self.get_position();
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
            let end_pos = self.get_position();
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
            if matches!(self.char0, Some('0'..='9')) {
                let number = self.lex_int_number(self.get_position(), false)?;
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
            if let Some(character) = self.take_number(radix) {
                value_text.push(character);
            } else if self.char0 == Some('_') && Lexer::<T>::is_digit_of_radix(self.char1, radix) {
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
        let take_char = Lexer::<T>::is_digit_of_radix(self.char0, radix);

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
        let kind = match (self.char0, self.char1) {
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
        let start_position = self.get_position();
        while Some('\n') != self.char0 {
            match self.char0 {
                Some(character) => content.push(character),
                None => break,
            }
            let _ = self.next_char();
        }
        let end_position = self.get_position();
        let token = match kind {
            Kind::Comment => Token::CommentNormal,
            Kind::Doc => Token::CommentDoc { content },
            Kind::ModuleDoc => Token::CommentModule,
        };
        (start_position, token, end_position)
    }

    fn lex_string(&mut self) -> LexResult {
        let start_position = self.get_position();
        // advance past the first quote
        let _ = self.next_char();
        let mut string_content = String::new();

        loop {
            match self.next_char() {
                Some('\\') => {
                    let slash_position = self.get_position() - 1;
                    if let Some(character) = self.char0 {
                        match character {
                            'f' | 'n' | 'r' | 't' | '"' | '\\' => {
                                let _ = self.next_char();
                                string_content.push('\\');
                                string_content.push(character);
                            }
                            'u' => {
                                let _ = self.next_char();

                                if self.char0 != Some('{') {
                                    return Err(LexicalError {
                                        error: LexicalErrorType::InvalidUnicodeEscape(
                                            InvalidUnicodeEscapeError::MissingOpeningBrace,
                                        ),
                                        location: SrcSpan {
                                            start: self.get_position() - 1,
                                            end: self.get_position(),
                                        },
                                    });
                                }

                                // All digits inside \u{...}.
                                let mut hex_digits = String::new();

                                loop {
                                    let _ = self.next_char();

                                    let Some(chr) = self.char0 else {
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
                                                start: self.get_position(),
                                                end: self.get_position() + 1,
                                            },
                                        });
                                    }
                                }

                                if self.char0 != Some('}') {
                                    return Err(LexicalError {
                                        error: LexicalErrorType::InvalidUnicodeEscape(
                                            InvalidUnicodeEscapeError::ExpectedHexDigitOrCloseBrace,
                                        ),
                                        location: SrcSpan {
                                            start: self.get_position() - 1,
                                            end: self.get_position(),
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
                                            start: slash_position,
                                            end: self.get_position(),
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
                                            start: slash_position,
                                            end: self.get_position(),
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
                                        start: slash_position,
                                        end: slash_position + 1,
                                    },
                                });
                            }
                        }
                    } else {
                        return Err(LexicalError {
                            error: LexicalErrorType::BadStringEscape,
                            location: SrcSpan {
                                start: slash_position,
                                end: slash_position,
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
                            start: start_position,
                            end: start_position,
                        },
                    });
                }
            }
        }
        let end_position = self.get_position();

        let token = Token::String {
            value: string_content.into(),
        };

        Ok((start_position, token, end_position))
    }

    fn is_name_start(&self, character: char) -> bool {
        matches!(character, '_' | 'a'..='z')
    }
    fn is_upname_start(&self, character: char) -> bool {
        character.is_ascii_uppercase()
    }
    fn is_number_start(&self, character: char, character1: Option<char>) -> bool {
        match character {
            '0'..='9' => true,
            '-' => matches!(character1, Some('0'..='9')),
            _ => false,
        }
    }

    fn is_name_continuation(&self) -> bool {
        self.char0
            .map(|c| matches!(c, '_' | '0'..='9' | 'a'..='z' | 'A'..='Z'))
            .unwrap_or(false)
    }

    // advance the stream and emit a token
    fn eat_single_char(&mut self, token: Token) {
        let token_start = self.get_position();
        let _ = self.next_char().expect("eat_single_char");
        let token_end = self.get_position();
        self.emit((token_start, token, token_end));
    }

    // Helper function to go to the next character coming up.
    fn next_char(&mut self) -> Option<char> {
        let character = self.char0;
        let next = match self.chars.next() {
            Some((location, character)) => {
                self.location0 = self.location1;
                self.location1 = location;
                Some(character)
            }
            None => {
                // EOF needs a single advance
                self.location0 = self.location1;
                self.location1 += 1;
                None
            }
        };
        self.char0 = self.char1;
        self.char1 = next;
        character
    }

    // Helper function to retrieve the current position.
    fn get_position(&self) -> u32 {
        self.location0
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
