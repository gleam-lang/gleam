// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! Lexer for semver ranges.
//!
//! Breaks a string of input into an iterator of tokens that can be used with a parser.
//!
//! Based off https://github.com/steveklabnik/semver-parser/blob/bee9de80aaa9653c5eb46a83658606cb21151e65/src/lexer.rs
//!
use self::Error::*;
use self::Token::*;
use std::str;

macro_rules! scan_while {
    ($self:expr, $start:expr, $first:pat_param $(| $rest:pat)*) => {{
        let mut __end = $start;

        loop {
            if let Some((idx, character)) = $self.one() {
                __end = idx;

                match character {
                    $first $(| $rest)* => $self.step(),
                    _ => break,
                }

                continue;
            } else {
                __end = $self.input.len();
            }

            break;
        }

        __end
    }}
}

/// Semver tokens.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Token<'input> {
    /// `==`
    Eq,
    /// `!=`
    NotEq,
    /// `>`
    Gt,
    /// `<`
    Lt,
    /// `<=`
    LtEq,
    /// `>=`
    GtEq,
    /// '~>`
    Pessimistic,
    /// `.`
    Dot,
    /// `-`
    Hyphen,
    /// `+`
    Plus,
    /// 'or'
    Or,
    /// 'and'
    And,
    /// Any number of whitespace (`\t\r\n `) and its span.
    Whitespace(usize, usize),
    /// Numeric component, like `0` or `42`.
    Numeric(u32),
    /// Alphanumeric component, like `alpha1` or `79deadbe`.
    AlphaNumeric(&'input str),
    /// An alphanumeric component with a leading zero, like `0alpha1` or `079deadbe`.
    LeadingZero(&'input str),
}

#[cfg(test)]
impl<'input> Token<'input> {
    /// Check if the current token is a whitespace token.
    pub fn is_whitespace(&self) -> bool {
        match *self {
            Whitespace(..) => true,
            _ => false,
        }
    }
}

impl std::fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Eq => write!(f, "=="),
            NotEq => write!(f, "!="),
            Gt => write!(f, ">"),
            Lt => write!(f, "<"),
            LtEq => write!(f, "<="),
            GtEq => write!(f, ">="),
            Pessimistic => write!(f, "~>"),
            Dot => write!(f, "."),
            Hyphen => write!(f, "-"),
            Plus => write!(f, "+"),
            Or => write!(f, "or"),
            And => write!(f, "and"),
            Whitespace(_, _) => write!(f, " "),
            Numeric(number) => write!(f, "{number}"),
            AlphaNumeric(string) => write!(f, "{string}"),
            LeadingZero(string) => write!(f, "{string}"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, thiserror::Error)]
pub enum Error {
    #[error("Unexpected character {0}")]
    UnexpectedCharacter(char),
}

/// Lexer for semver tokens belonging to a range.
#[derive(Debug)]
pub struct Lexer<'input> {
    input: &'input str,
    chars: str::CharIndices<'input>,
    // Lookahead
    char0: Option<(usize, char)>,
    char1: Option<(usize, char)>,
}

impl<'input> Lexer<'input> {
    /// Construct a new lexer for the given input.
    pub fn new(input: &str) -> Lexer<'_> {
        let mut chars = input.char_indices();
        let char0 = chars.next();
        let char1 = chars.next();

        Lexer {
            input,
            chars,
            char0,
            char1,
        }
    }

    /// Shift all lookahead storage by one.
    fn step(&mut self) {
        self.char0 = self.char1;
        self.char1 = self.chars.next();
    }

    fn step_n(&mut self, n: usize) {
        for _ in 0..n {
            self.step();
        }
    }

    /// Access the one character, or set it if it is not set.
    fn one(&mut self) -> Option<(usize, char)> {
        self.char0
    }

    /// Access two characters.
    fn two(&mut self) -> Option<(usize, char, char)> {
        self.char0
            .and_then(|(start, char0)| self.char1.map(|(_, char1)| (start, char0, char1)))
    }

    /// Consume a component.
    ///
    /// A component can either be an alphanumeric or numeric.
    /// Does not permit leading zeroes.
    fn component(&mut self, start: usize) -> Result<Token<'input>, Error> {
        let end = scan_while!(self, start, '0'..='9' | 'A'..='Z' | 'a'..='z');
        let input = &self.input[start..end];

        let mut chars = input.chars();
        let (char0, char1) = (chars.next(), chars.next());

        // Exactly zero
        if char0 == Some('0') && char1.is_none() {
            return Ok(Numeric(0));
        }

        if let Ok(number) = input.parse::<u32>() {
            // Only parse as a number if there is no leading zero
            if char0 != Some('0') {
                return Ok(Numeric(number));
            } else {
                return Ok(LeadingZero(input));
            }
        }

        Ok(AlphaNumeric(input))
    }

    fn and(&mut self, start: usize) -> Result<Token<'input>, Error> {
        match self.one() {
            Some((_, 'd')) => {
                self.step();
                Ok(And)
            }
            _ => self.component(start),
        }
    }

    /// Consume whitespace.
    fn whitespace(&mut self, start: usize) -> Result<Token<'input>, Error> {
        let end = scan_while!(self, start, ' ' | '\t' | '\n' | '\r');
        Ok(Whitespace(start, end))
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Result<Token<'input>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        #[allow(clippy::never_loop)]
        loop {
            // Two subsequent char tokens.
            if let Some((start, char0, char1)) = self.two() {
                let double_character_token = match (char0, char1) {
                    ('~', '>') => Some(Pessimistic),
                    ('!', '=') => Some(NotEq),
                    ('<', '=') => Some(LtEq),
                    ('>', '=') => Some(GtEq),
                    ('=', '=') => Some(Eq),
                    ('o', 'r') => Some(Or),
                    ('a', 'n') => {
                        self.step_n(2);
                        return Some(self.and(start));
                    }
                    _ => None,
                };

                if let Some(two) = double_character_token {
                    self.step_n(2);
                    return Some(Ok(two));
                }
            }

            // Single char and start of numeric tokens.
            if let Some((start, char2)) = self.one() {
                let token = match char2 {
                    ' ' | '\t' | '\n' | '\r' => {
                        self.step();
                        return Some(self.whitespace(start));
                    }
                    '>' => Gt,
                    '<' => Lt,
                    '.' => Dot,
                    '-' => Hyphen,
                    '+' => Plus,
                    '0'..='9' | 'a'..='z' | 'A'..='Z' => {
                        self.step();
                        return Some(self.component(start));
                    }
                    character => return Some(Err(UnexpectedCharacter(character))),
                };

                self.step();
                return Some(Ok(token));
            };

            return None;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex(input: &str) -> Vec<Token<'_>> {
        Lexer::new(input).map(Result::unwrap).collect::<Vec<_>>()
    }

    #[test]
    pub fn simple_tokens() {
        assert_eq!(
            lex("!===><<=>=~>.-+orand"),
            vec![
                NotEq,
                Eq,
                Gt,
                Lt,
                LtEq,
                GtEq,
                Pessimistic,
                Dot,
                Hyphen,
                Plus,
                Or,
                And
            ]
        );
    }

    #[test]
    pub fn whitespace() {
        assert_eq!(
            lex("  wibble \t\n\rwobble"),
            vec![
                Whitespace(0, 2),
                AlphaNumeric("wibble"),
                Whitespace(8, 12),
                AlphaNumeric("wobble"),
            ]
        );
    }

    #[test]
    pub fn components() {
        assert_eq!(lex("42"), vec![Numeric(42)]);
        assert_eq!(lex("0"), vec![Numeric(0)]);
        assert_eq!(lex("5885644aa"), vec![AlphaNumeric("5885644aa")]);
        assert_eq!(lex("beta2"), vec![AlphaNumeric("beta2")]);
        assert_eq!(lex("beta.2"), vec![AlphaNumeric("beta"), Dot, Numeric(2)]);
    }

    #[test]
    pub fn empty() {
        assert_eq!(lex(""), vec![]);
    }

    #[test]
    pub fn numeric_all_numbers() {
        let expected: Vec<Token> = vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
            .into_iter()
            .map(Numeric)
            .collect::<Vec<_>>();

        let actual: Vec<_> = lex("0 1 2 3 4 5 6 7 8 9")
            .into_iter()
            .filter(|t| !t.is_whitespace())
            .collect();

        assert_eq!(actual, expected);
    }

    #[test]
    pub fn token_display() {
        assert_eq!(Eq.to_string(), "==");
        assert_eq!(NotEq.to_string(), "!=");
        assert_eq!(Gt.to_string(), ">");
        assert_eq!(Lt.to_string(), "<");
        assert_eq!(LtEq.to_string(), "<=");
        assert_eq!(GtEq.to_string(), ">=");
        assert_eq!(Pessimistic.to_string(), "~>");
        assert_eq!(Dot.to_string(), ".");
        assert_eq!(Hyphen.to_string(), "-");
        assert_eq!(Plus.to_string(), "+");
        assert_eq!(Or.to_string(), "or");
        assert_eq!(And.to_string(), "and");
        assert_eq!(Whitespace(0, 1).to_string(), " ");
        assert_eq!(Numeric(42).to_string(), "42");
        assert_eq!(AlphaNumeric("abc").to_string(), "abc");
        assert_eq!(LeadingZero("012").to_string(), "012");
    }
}
