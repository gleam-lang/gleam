use std::cmp::Ordering::{Equal, Greater, Less};

use super::{
    Identifier::{AlphaNumeric, Numeric},
    *,
};

// Tests adapted from the tests for Elixir's version module

macro_rules! version_parse_test {
    ($name:ident, $input:expr, $major:expr, $minor:expr, $patch:expr,$pre:expr, $build:expr) => {
        #[test]
        fn $name() {
            assert_eq!(
                Version::parse($input).unwrap(),
                Version {
                    major: $major,
                    minor: $minor,
                    patch: $patch,
                    pre: $pre,
                    build: $build
                }
            );
        }
    };

    ($name:ident, $input:expr, $major:expr, $minor:expr, $patch:expr, $pre:expr) => {
        #[test]
        fn $name() {
            assert_eq!(
                Version::parse($input).unwrap(),
                Version {
                    major: $major,
                    minor: $minor,
                    patch: $patch,
                    pre: $pre,
                    build: None
                }
            );
        }
    };

    ($name:ident, $input:expr, $major:expr, $minor:expr, $patch:expr) => {
        #[test]
        fn $name() {
            assert_eq!(
                Version::parse($input).unwrap(),
                Version {
                    major: $major,
                    minor: $minor,
                    patch: $patch,
                    pre: vec![],
                    build: None
                }
            );
        }
    };
}

macro_rules! version_parse_fail_test {
    ($name:ident, $input:expr) => {
        #[test]
        fn $name() {
            println!("{}", $input);
            Version::parse($input).unwrap_err();
        }
    };
}

macro_rules! version_parse_print {
    ($name:ident, $input:expr) => {
        #[test]
        fn $name() {
            assert_eq!($input, Version::parse($input).unwrap().to_string().as_str());
        }
    };
}

version_parse_test!(triplet, "1.2.3", 1, 2, 3);

version_parse_test!(
    build,
    "1.4.5+ignore",
    1,
    4,
    5,
    vec![],
    Some("ignore".to_string())
);

version_parse_test!(
    two_part_build,
    "0.0.1+sha.0702245",
    0,
    0,
    1,
    vec![],
    Some("sha.0702245".to_string())
);

version_parse_test!(
    pre,
    "1.4.5-6-g3318bd5",
    1,
    4,
    5,
    vec![AlphaNumeric("6-g3318bd5".to_string())]
);

version_parse_test!(
    multi_part_pre,
    "1.4.5-6.7.eight",
    1,
    4,
    5,
    vec![Numeric(6), Numeric(7), AlphaNumeric("eight".to_string())]
);

version_parse_test!(
    pre_and_build,
    "1.4.5-6-g3318bd5+ignore",
    1,
    4,
    5,
    vec![AlphaNumeric("6-g3318bd5".to_string())],
    Some("ignore".to_string())
);

version_parse_fail_test!(just_a_word, "foobar");

version_parse_fail_test!(just_major, "2");

version_parse_fail_test!(major_dor, "2.");

version_parse_fail_test!(major_minor, "2.3");

version_parse_fail_test!(major_minor_dot, "2.3.");

version_parse_fail_test!(triplet_dash, "2.3.0-");

version_parse_fail_test!(triplet_plus, "2.3.0+");

version_parse_fail_test!(triplet_dot, "2.3.0.");

version_parse_fail_test!(quad, "2.3.0.4");

version_parse_fail_test!(missing_minor, "2.3.-rc.1");

version_parse_fail_test!(missing_minor_with_dot, "2.3.+rc.1");

version_parse_fail_test!(zero_pre, "2.3.0-01");

version_parse_fail_test!(double_zero_pre, "2.3.00-1");

version_parse_fail_test!(double_zero, "2.3.00");

version_parse_fail_test!(leading_zero_minor, "2.03.0");

version_parse_fail_test!(leading_zero_major, "02.3.0");

version_parse_fail_test!(extra_whitespace, "0. 0.0");

version_parse_fail_test!(and_in_version, "0.1.0-andpre");

version_parse_print!(print_triplet, "1.100.1000");

version_parse_print!(print_pre, "1.100.4-dev");

version_parse_print!(print_pre_dot, "1.100.4-dev.1.r.t");

version_parse_print!(print_build, "1.100.4+dev.1.r.t");

version_parse_print!(print_pre_build, "1.100.4-ewfjhwefj.wefw.w.1.ff+dev.1.r.t");

macro_rules! parse_range_test {
    ($name:ident, $input:expr, $expected:expr) => {
        #[test]
        fn $name() {
            assert_eq!(Version::parse_range($input).unwrap(), $expected);
        }
    };
}

macro_rules! parse_range_fail_test {
    ($name:ident, $input:expr) => {
        #[test]
        fn $name() {
            Version::parse_range($input).unwrap_err();
        }
    };
}

fn v(a: u32, b: u32, c: u32) -> Version {
    Version::new(a, b, c)
}

fn v_(major: u32, minor: u32, patch: u32, pre: Vec<Identifier>, build: Option<String>) -> Version {
    Version {
        major,
        minor,
        patch,
        pre,
        build,
    }
}

parse_range_test!(leading_space, " 1.2.3", Range::exact(v(1, 2, 3)));
parse_range_test!(trailing_space, "1.2.3 ", Range::exact(v(1, 2, 3)));

parse_range_test!(eq_triplet, "== 1.2.3 ", Range::exact(v(1, 2, 3)));

parse_range_test!(eq_triplet_nospace, "==1.2.3 ", Range::exact(v(1, 2, 3)));

parse_range_test!(
    neq_triplet,
    "!= 1.2.3",
    Range::strictly_lower_than(v(1, 2, 3)).union(&Range::higher_than(v(1, 2, 4)))
);

parse_range_test!(implicit_eq, "2.2.3", Range::exact(v(2, 2, 3)));

parse_range_test!(
    range_pre_build,
    "1.2.3-thing+oop",
    Range::exact(v_(
        1,
        2,
        3,
        vec![Identifier::AlphaNumeric("thing".to_string())],
        Some("oop".to_string())
    ))
);

parse_range_test!(
    and,
    "< 1.2.3 and > 1.0.1",
    Range::strictly_lower_than(v(1, 2, 3)).intersection(&Range::higher_than(v(1, 0, 2)))
);

parse_range_test!(
    or,
    "< 1.2.3 or > 1.0.1",
    Range::strictly_lower_than(v(1, 2, 3)).union(&Range::higher_than(v(1, 0, 2)))
);

parse_range_test!(gt, "> 1.0.0", Range::higher_than(v(1, 0, 1)));
parse_range_test!(gt_eq, ">= 1.0.0", Range::higher_than(v(1, 0, 0)));
parse_range_test!(lt, "< 1.0.0", Range::strictly_lower_than(v(1, 0, 0)));
parse_range_test!(lt_eq, "<= 1.0.0", Range::strictly_lower_than(v(1, 0, 1)));

parse_range_test!(
    pessimistic_pair,
    "~> 2.2",
    Range::higher_than(v(2, 2, 0)).intersection(&Range::strictly_lower_than(v(3, 0, 0)))
);

parse_range_test!(
    pessimistic_triplet,
    "~> 4.6.5",
    Range::higher_than(v(4, 6, 5)).intersection(&Range::strictly_lower_than(v(4, 7, 0)))
);

parse_range_test!(
    pessimistic_triplet_pre,
    "~> 4.6.5-eee",
    Range::higher_than(v_(
        4,
        6,
        5,
        vec![Identifier::AlphaNumeric("eee".to_string())],
        None,
    ))
    .intersection(&Range::strictly_lower_than(v(4, 7, 0)))
);

parse_range_test!(
    pessimistic_triplet_build,
    "~> 4.6.5+eee",
    Range::higher_than(v_(4, 6, 5, vec![], Some("eee".to_string())))
        .intersection(&Range::strictly_lower_than(v(4, 7, 0)))
);

parse_range_fail_test!(range_quad, "1.1.1.1");
parse_range_fail_test!(range_just_major, "1");
parse_range_fail_test!(range_just_major_minor, "1.1");
parse_range_fail_test!(alpha_component, "1.1.a");

parse_range_fail_test!(range_word, "foobar");
parse_range_fail_test!(range_major_dot, "2.");
parse_range_fail_test!(range_major_minor_dot, "2.3.");
parse_range_fail_test!(range_triplet_dash, "2.3.0-");
parse_range_fail_test!(range_triplet_plus, "2.3.0+");
parse_range_fail_test!(range_triplet_dot, "2.3.0.");
parse_range_fail_test!(range_dot_dash, "2.3.-rc.1");
parse_range_fail_test!(range_dot_plus, "2.3.+rc.1");
parse_range_fail_test!(range_zero_pre, "2.3.0-01");
parse_range_fail_test!(patch_zerozero_dash, "2.3.00-1");
parse_range_fail_test!(patch_zerozero, "2.3.00");
parse_range_fail_test!(minor_leading_zero, "2.03.0");
parse_range_fail_test!(major_leading_zero, "02.3.0");
parse_range_fail_test!(triplet_containing_whitespace, "0. 0.0");
parse_range_fail_test!(dash_amp, "0.1.0-&&pre");

parse_range_fail_test!(or_whitespace_before, "!= 1.2.3or == 1.0.1");
parse_range_fail_test!(or_whitespace_after, "!= 1.2.3 or== 1.0.1");
parse_range_fail_test!(and_whitespace_before, "!= 1.2.3and == 1.0.1");
parse_range_fail_test!(and_whitespace_after, "!= 1.2.3 and== 1.0.1");

parse_range_fail_test!(trailing_and, "1.1.1 and");
parse_range_fail_test!(trailing_or, "1.1.1 or");
parse_range_fail_test!(leading_and, "and 1.1.1");
parse_range_fail_test!(leading_or, "and 1.1.1");
parse_range_fail_test!(just_and, "and");
parse_range_fail_test!(just_or, "and");

parse_range_fail_test!(duplicate_eq, "== ==");
parse_range_fail_test!(just_eq, "==");
parse_range_fail_test!(empty, "");

parse_range_fail_test!(pessimistic_major, "~> 1");

macro_rules! assert_order {
    ($name:ident, $left:expr, $ord:expr, $right:expr) => {
        #[test]
        fn $name() {
            let left = Version::parse($left);
            let right = Version::parse($right);
            assert_eq!(left.cmp(&right), $ord)
        }
    };
}

assert_order!(ord_same, "1.0.0", Equal, "1.0.0");
assert_order!(ord_same_build_right, "1.0.0", Equal, "1.0.0+1");
assert_order!(ord_same_build_left, "1.0.0+1", Equal, "1.0.0");
assert_order!(ord_same_diff_build, "1.0.0+1", Equal, "1.0.0+2");

assert_order!(ord_diff_build, "1.0.0+2", Equal, "1.0.0+1");

assert_order!(ord_major_greater, "3.0.0", Greater, "2.0.0");
assert_order!(ord_major_lesser, "1.0.0", Less, "2.0.0");

assert_order!(ord_minor_greater, "1.1.0", Greater, "1.0.0");
assert_order!(ord_minor_lesser, "1.0.0", Less, "1.1.0");

assert_order!(ord_patch_greater, "1.0.1", Greater, "1.0.0");
assert_order!(ord_patch_lesser, "1.0.0", Less, "1.0.1");

assert_order!(ord_pre_smaller_than_zero, "1.0.0", Greater, "1.0.0-rc1");
assert_order!(ord_pre_smaller_than_zero_flip, "1.0.0-rc1", Less, "1.0.0");

assert_order!(ord_pre_rc1_2, "1.0.0-rc1", Less, "1.0.0-rc2");
