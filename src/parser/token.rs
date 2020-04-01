#[cfg(test)]
mod tests;

use unicode_segmentation::{GraphemeIndices, UnicodeSegmentation};

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;
pub type SpannedToken<'input> = Spanned<Token<'input>, usize, ()>;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Token<'input> {
    PositiveNumber(&'input str),
}

impl<'input> Token<'input> {
    fn len(&self) -> usize {
        match self {
            Self::PositiveNumber(s) => s.len(),
        }
    }
}

pub struct Lexer<'input> {
    graphemes: std::iter::Peekable<GraphemeIndices<'input>>,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Lexer {
            graphemes: UnicodeSegmentation::grapheme_indices(input, true).peekable(),
        }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Token<'input>, usize, ()>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.graphemes.next() {
            None | Some((_, " ")) | Some((_, "\t")) => None,
            Some((i, c)) => t(i, Token::PositiveNumber(c)),
        }
    }
}

fn t<'input>(i: usize, t: Token<'input>) -> Option<SpannedToken<'input>> {
    Some(Ok((i, t, i + t.len())))
}
