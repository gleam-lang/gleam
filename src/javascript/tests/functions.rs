use crate::assert_js;

use super::CURRENT_PACKAGE;

#[test]
fn exported_functions() {
    assert_js!(
        r#"
pub fn add(x, y) {
    x + y
}"#,
        r#""use strict";

export function add(x, y) {
  return x + y;
}
"#
    );
}

#[test]
fn calling_functions() {
    assert_js!(
        r#"
pub fn twice(f: fn(t) -> t, x: t) -> t {
    f(f(x))
}
pub fn add_one(x: Int) -> Int {
    x + 1
}
pub fn add_two(x: Int) -> Int {
    twice(add_one, x)
}

pub fn take_two(x: Int) -> Int {
    twice(fn(y) {y - 1}, x)
}
"#,
        r#""use strict";

export function twice(f, x) {
  return f(f(x));
}

export function add_one(x) {
  return x + 1;
}

export function add_two(x) {
  return twice(add_one, x);
}

export function take_two(x) {
  return twice((y) => { return y - 1; }, x);
}
"#
    );
}

#[test]
fn function_formatting() {
    assert_js!(
        r#"
pub fn add(the_first_variable_that_should_be_added, the_second_variable_that_should_be_added) {
  the_first_variable_that_should_be_added + the_second_variable_that_should_be_added
}"#,
        r#""use strict";

export function add(
  the_first_variable_that_should_be_added,
  the_second_variable_that_should_be_added
) {
  return the_first_variable_that_should_be_added + the_second_variable_that_should_be_added;
}
"#
    );

    assert_js!(
        r#"
pub fn this_function_really_does_have_a_ludicrously_unfeasibly_long_name_for_a_function(x, y) {
x + y
}"#,
        r#""use strict";

export function this_function_really_does_have_a_ludicrously_unfeasibly_long_name_for_a_function(
  x,
  y
) {
  return x + y;
}
"#
    );

    assert_js!(
        r#"
pub fn add(x, y) {
x + y
}

pub fn long() {
  add(1, add(1, add(1, add(1, add(1, add(1, add(1, add(1, add(1, add(1, add(1, add(1, add(1, add(1, add(1, 1)))))))))))))))
}"#,
        r#""use strict";

export function add(x, y) {
  return x + y;
}

export function long() {
  return add(
    1,
    add(
      1,
      add(
        1,
        add(
          1,
          add(
            1,
            add(
              1,
              add(
                1,
                add(
                  1,
                  add(1, add(1, add(1, add(1, add(1, add(1, add(1, 1))))))),
                ),
              ),
            ),
          ),
        ),
      ),
    ),
  );
}
"#
    );

    assert_js!(
        r#"
pub fn math(x, y) {
  fn() {
    x + y
    x - y
    2 * x
  }
}"#,
        r#""use strict";

export function math(x, y) {
  return () => {
    x + y;
    x - y;
    return Math.imul(2, x);
  };
}
"#
    );
}

#[test]
fn tail_call() {
    assert_js!(
        r#"
pub fn count(xs, n) {
  case xs {
    [] -> n
    [_, ..xs] -> count(xs, n + 1)
  }
}
"#,
        r#""use strict";

export function count(xs, n) {
  while (true) {
    if (xs?.length === 0) {
      return n;
    } else if (xs?.[1]?.length !== undefined) {
      let xs$1 = xs[1];
      xs = xs$1;
      n = n + 1;
    } else {
      throw new Error("Bad match");
    }
  }
}
"#
    );
}

#[test]
fn tail_call_doesnt_clobber_tail_position_tracking() {
    assert_js!(
        r#"
pub fn loop(indentation) {
  case indentation > 0 {
    True -> loop(indentation - 1)
    False -> Nil
  }
}
"#,
        r#""use strict";

export function loop(indentation) {
  while (true) {
    let $ = indentation > 0;
    if ($) {
      indentation = indentation - 1;
    } else if (!$) {
      return undefined;
    } else {
      throw new Error("Bad match");
    }
  }
}
"#
    );
}

#[test]
fn pipe_last() {
    assert_js!(
        r#"fn id(x) { x }
pub fn main() {
  1
  |> id
}
"#,
        r#""use strict";

function id(x) {
  return x;
}

export function main() {
  return id(1);
}
"#
    );
}

#[test]
fn calling_fn_literal() {
    assert_js!(
        r#"pub fn main() {
  fn(x) { x }(1)
}
"#,
        r#""use strict";

export function main() {
  return ((x) => { return x; })(1);
}
"#
    );
}

// Don't mistake calling a function with the same name as the current function
// as tail recursion
#[test]
fn shadowing_current() {
    assert_js!(
        r#"pub fn main() {
  let main = fn() { 0 }
  main()
}
"#,
        r#""use strict";

export function main() {
  let main$1 = () => { return 0; };
  return main$1();
}
"#
    );
}

#[test]
fn recursion_with_discards() {
    assert_js!(
        r#"pub fn main(f, _) {
  f()
  main(f, 1)
}
"#,
        r#""use strict";

export function main(f, _) {
  while (true) {
    f();
    f = f;
    1;
  }
}
"#
    );
}

#[test]
fn no_recur_in_anon_fn() {
    assert_js!(
        r#"pub fn main() {
  fn() { main() }
  1
}
"#,
        r#""use strict";

export function main() {
  () => { return main(); };
  return 1;
}
"#
    );
}

#[test]
fn case_in_call() {
    assert_js!(
        r#"pub fn main(f, x) {
  f(case x {
    1 -> 2
    _ -> 0
  })
}
"#,
        r#""use strict";

export function main(f, x) {
  return f(
    (() => {
      if (x === 1) {
        return 2;
      } else {
        return 0;
      }
    })(),
  );
}
"#
    );
}

#[test]
fn reserved_word_fn() {
    assert_js!(
        r#"pub fn class() {
  Nil
}
"#,
        r#""use strict";

export function class$() {
  return undefined;
}
"#
    );
}

#[test]
fn reserved_word_imported() {
    assert_js!(
        (
            CURRENT_PACKAGE,
            vec!["for".to_string()],
            "pub fn class() { 1 }"
        ),
        r#"import for.{class}

pub fn export() {
  class()
}
"#,
        r#""use strict";

import * as for$ from "./for.js";
const { class$ } = for$;

export function export$() {
  return class$();
}
"#
    );
}

#[test]
fn reserved_word_imported_alias() {
    assert_js!(
        (
            CURRENT_PACKAGE,
            vec!["for".to_string()],
            "pub fn class() { 1 }"
        ),
        r#"import for.{class as while} as function

pub fn export() {
  let delete = function.class
  while()
}
"#,
        r#""use strict";

import * as function$ from "./for.js";
const { class$: while$ } = function$;

export function export$() {
  let delete$ = function$.class$;
  return while$();
}
"#
    );
}

#[test]
fn reserved_word_const() {
    assert_js!(
        r#"const in = 1

pub fn export() {
  in
}
"#,
        r#""use strict";

const in$ = 1;

export function export$() {
  return in$;
}
"#
    );
}
