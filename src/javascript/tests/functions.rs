use crate::assert_js;

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
    return 2 * x; };
}
"#
    );
}
