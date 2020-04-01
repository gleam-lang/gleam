use super::*;

#[test]
fn lexer_test() {
    macro_rules! assert_tokens {
        ($src:expr, $tokens:expr $(,)?) => {
            let expected: &'static [Token<'static>] = $tokens;
            let lexer = Lexer::new($src);
            let tokens: Result<Vec<Token<'_>>, ()> = lexer.map(|r| r.map(|(_, t, _)| t)).collect();
            assert_eq!(expected, tokens.unwrap().as_slice());
        };
    }

    assert_tokens!("", &[]);
    assert_tokens!(" ", &[]);
    assert_tokens!("\t", &[]);
}
