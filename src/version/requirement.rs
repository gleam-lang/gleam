use super::{
    parser::{self, Parser},
    Version,
};

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Requirement(pub OneOf<AllOf<Comparator>>);

impl Requirement {
    pub fn parse(input: &str) -> Result<Self, parser::Error> {
        let mut parser = Parser::new(input)?;
        let requirement = parser.requirement()?;
        if !parser.is_eof() {
            return Err(parser::Error::MoreInput(parser.tail()?));
        }
        Ok(requirement)
    }
}

pub type OneOf<T> = Vec<T>;
pub type AllOf<T> = Vec<T>;

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Comparator {
    pub operator: Operator,
    pub version: Version,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Operator {
    Lt,
    Lte,
    Gt,
    Gte,
    Eq,
    NotEq,
}

#[cfg(test)]
mod tests {
    use super::{Operator::*, *};

    fn comp(operator: Operator, major: u64, minor: u64, patch: u64) -> Comparator {
        Comparator {
            operator,
            version: Version {
                major,
                minor,
                patch,
                pre: vec![],
                build: None,
            },
        }
    }

    macro_rules! parse_test {
        ($name:ident, $input:expr, $expected:expr) => {
            #[test]
            fn $name() {
                assert_eq!(Requirement::parse($input).unwrap(), Requirement($expected));
            }
        };
    }

    macro_rules! parse_fail_test {
        ($name:ident, $input:expr) => {
            #[test]
            fn $name() {
                Requirement::parse($input).unwrap_err();
            }
        };
    }

    parse_test!(leading_space, " == 1.2.3", vec![vec![comp(Eq, 1, 2, 3)]]);

    parse_test!(trailing_space, "== 1.2.3 ", vec![vec![comp(Eq, 1, 2, 3)]]);

    parse_test!(eq_triplet, "== 1.2.3", vec![vec![comp(Eq, 1, 2, 3)]]);

    parse_test!(eq_triplet_nospace, "==1.2.3", vec![vec![comp(Eq, 1, 2, 3)]]);

    parse_test!(neq_triplet, "!= 1.2.3", vec![vec![comp(NotEq, 1, 2, 3)]]);

    parse_test!(implicit_eq, "1.2.3", vec![vec![comp(Eq, 1, 2, 3)]]);

    parse_test!(
        and,
        "!= 1.2.3 and == 1.0.1",
        vec![vec![comp(NotEq, 1, 2, 3), comp(Eq, 1, 0, 1)]]
    );

    parse_test!(
        or,
        "!= 1.2.3 or 1.0.1",
        vec![vec![comp(NotEq, 1, 2, 3)], vec![comp(Eq, 1, 0, 1)]]
    );

    parse_fail_test!(or_whitespace_before, "!= 1.2.3or == 1.0.1");
    parse_fail_test!(or_whitespace_after, "!= 1.2.3 or== 1.0.1");
    parse_fail_test!(and_whitespace_before, "!= 1.2.3and == 1.0.1");
    parse_fail_test!(and_whitespace_after, "!= 1.2.3 and== 1.0.1");
    parse_fail_test!(duplicate_eq, "== ==");
}
