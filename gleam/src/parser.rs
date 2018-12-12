use crate::ast::*;

// https://github.com/Marwes/combine

use combine::parser::char;
use combine::*;

fn raw_int<I>() -> impl Parser<Input = I, Output = isize>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (
        optional(char::char('-')),
        many1(char::digit()).map(|i: String| i.parse().unwrap_or(0)),
    )
        .map(|(sign, i)| match sign {
            Some(_) => 0 - i,
            None => i,
        })
}

fn raw_float<I>() -> impl Parser<Input = I, Output = f64>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let parse_float =
        |(a, _, b): (String, char, String)| [a, ".".to_string(), b].join("").parse().unwrap_or(0.0);

    (
        optional(char::char('-')),
        (many1(char::digit()), char::char('.'), many1(char::digit())).map(parse_float),
    )
        .map(|(sign, i)| match sign {
            Some(_) => 0.0 - i,
            None => i,
        })
}

#[test]
fn raw_test() {
    assert_eq!(Ok((1234, "")), raw_int().easy_parse("1234"));
    assert_eq!(Ok((-567, "")), raw_int().easy_parse("-567"));

    assert_eq!(Ok((1234.5, "")), raw_float().easy_parse("1234.5"));
    assert_eq!(Ok((-678.9, "")), raw_float().easy_parse("-678.9"));
}
