use std::{convert::TryFrom, fmt};

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

impl<'a> TryFrom<&'a str> for Requirement {
    type Error = parser::Error<'a>;

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        Self::parse(value)
    }
}

impl fmt::Display for Requirement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, alternative) in self.0.iter().enumerate() {
            if i != 0 {
                write!(f, " or ")?;
            }
            for (i, comparator) in alternative.iter().enumerate() {
                if i != 0 {
                    write!(f, " and ")?;
                }
                comparator.fmt(f)?;
            }
        }
        Ok(())
    }
}

pub type OneOf<T> = Vec<T>;
pub type AllOf<T> = Vec<T>;

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Comparator {
    pub operator: Operator,
    pub version: Version,
}

impl fmt::Display for Comparator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.operator, self.version)
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Operator {
    Lt,
    LtEq,
    Gt,
    GtEq,
    Eq,
    NotEq,
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operator::Lt => write!(f, "<"),
            Operator::LtEq => write!(f, "<="),
            Operator::Gt => write!(f, ">"),
            Operator::GtEq => write!(f, ">="),
            Operator::Eq => write!(f, "=="),
            Operator::NotEq => write!(f, "!="),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::version::Identifier;

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

    fn comp_(
        operator: Operator,
        major: u64,
        minor: u64,
        patch: u64,
        pre: Vec<Identifier>,
        build: Option<String>,
    ) -> Comparator {
        Comparator {
            operator,
            version: Version {
                major,
                minor,
                patch,
                pre,
                build,
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

    macro_rules! parse_print_test {
        ($name:ident, $input:expr) => {
            #[test]
            fn $name() {
                assert_eq!(
                    $input,
                    Requirement::parse($input).unwrap().to_string().as_str()
                );
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

    parse_test!(gt, "> 1.0.0", vec![vec![comp(Gt, 1, 0, 0)]]);
    parse_test!(gt_eq, ">= 1.0.0", vec![vec![comp(GtEq, 1, 0, 0)]]);
    parse_test!(lt, "< 1.0.0", vec![vec![comp(Lt, 1, 0, 0)]]);
    parse_test!(lt_eq, "<= 1.0.0", vec![vec![comp(LtEq, 1, 0, 0)]]);

    parse_test!(
        pessimistic_pair,
        "~> 2.2",
        vec![vec![comp(GtEq, 2, 2, 0), comp(Lt, 3, 0, 0)]]
    );

    parse_test!(
        pessimistic_triplet,
        "~> 4.6.5",
        vec![vec![comp(GtEq, 4, 6, 5), comp(Lt, 4, 7, 0)]]
    );

    parse_test!(
        pessimistic_triplet_pre,
        "~> 4.6.5-eee",
        vec![vec![
            comp_(
                GtEq,
                4,
                6,
                5,
                vec![Identifier::AlphaNumeric("eee".to_string())],
                None,
            ),
            comp(Lt, 4, 7, 0)
        ]]
    );

    parse_test!(
        pessimistic_triplet_build,
        "~> 4.6.5+eee22",
        vec![vec![
            comp_(GtEq, 4, 6, 5, vec![], Some("eee22".to_string()),),
            comp(Lt, 4, 7, 0)
        ]]
    );

    parse_test!(
        pessimistic_triplet_pre_build,
        "~> 4.6.5-whatever+eee22",
        vec![vec![
            comp_(
                GtEq,
                4,
                6,
                5,
                vec![Identifier::AlphaNumeric("whatever".to_string())],
                Some("eee22".to_string()),
            ),
            comp(Lt, 4, 7, 0)
        ]]
    );

    parse_fail_test!(quad, "1.1.1.1");
    parse_fail_test!(just_major, "1");
    parse_fail_test!(just_major_minor, "1.1");
    parse_fail_test!(alpha_component, "1.1.a");

    parse_fail_test!(word, "foobar");
    parse_fail_test!(major_dot, "2.");
    parse_fail_test!(major_minor_dot, "2.3.");
    parse_fail_test!(triplet_dash, "2.3.0-");
    parse_fail_test!(triplet_plus, "2.3.0+");
    parse_fail_test!(triplet_dot, "2.3.0.");
    parse_fail_test!(dot_dash, "2.3.-rc.1");
    parse_fail_test!(dot_plus, "2.3.+rc.1");
    parse_fail_test!(zero_pre, "2.3.0-01");
    parse_fail_test!(patch_zerozero_dash, "2.3.00-1");
    parse_fail_test!(patch_zerozero, "2.3.00");
    parse_fail_test!(minor_leading_zero, "2.03.0");
    parse_fail_test!(major_leading_zero, "02.3.0");
    parse_fail_test!(triplet_containing_whitespace, "0. 0.0");
    parse_fail_test!(dash_amp, "0.1.0-&&pre");

    parse_fail_test!(or_whitespace_before, "!= 1.2.3or == 1.0.1");
    parse_fail_test!(or_whitespace_after, "!= 1.2.3 or== 1.0.1");
    parse_fail_test!(and_whitespace_before, "!= 1.2.3and == 1.0.1");
    parse_fail_test!(and_whitespace_after, "!= 1.2.3 and== 1.0.1");

    parse_fail_test!(trailing_and, "1.1.1 and");
    parse_fail_test!(trailing_or, "1.1.1 or");
    parse_fail_test!(leading_and, "and 1.1.1");
    parse_fail_test!(leading_or, "and 1.1.1");
    parse_fail_test!(just_and, "and");
    parse_fail_test!(just_or, "and");

    parse_fail_test!(duplicate_eq, "== ==");
    parse_fail_test!(just_eq, "==");
    parse_fail_test!(empty, "");

    parse_fail_test!(pessimistic_major, "~> 1");

    parse_print_test!(print_and_or, "== 1.0.3 or > 1.0.3 and < 1.4.2");
}
