use crate::assert_js;

#[test]
fn type_() {
    assert_js!(
        r#"pub external type Thing"#,
        r#""use strict";
"#
    );
}

#[test]
fn module_fn() {
    assert_js!(
        r#"external fn show(anything) -> Nil = "utils" "inspect""#,
        r#""use strict";

import { inspect as show } from "utils";
"#
    );
}

#[test]
fn pub_module_fn() {
    assert_js!(
        r#"pub external fn show(anything) -> Nil = "utils" "inspect""#,
        r#""use strict";

import { inspect as show } from "utils";
export { show };
"#
    );
}

#[test]
fn global_fn() {
    assert_js!(
        r#"external fn down(Float) -> Float = "" "Math.floor""#,
        r#""use strict";

function down(arg0) {
  return Math.floor(arg0)
}
"#
    );
}

#[test]
fn pub_global_fn() {
    assert_js!(
        r#"pub external fn down(Float) -> Float = "" "Math.floor""#,
        r#""use strict";

export function down(arg0) {
  return Math.floor(arg0)
}
"#
    );
}

#[test]
fn same_name_global_external() {
    assert_js!(
        r#"pub external fn fetch(Nil) -> Nil = "" "fetch""#,
        r#""use strict";

export function fetch(arg0) {
  return globalThis.fetch(arg0)
}
"#
    );
}
