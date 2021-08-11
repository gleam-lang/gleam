use crate::assert_erl;

#[test]
fn excluded() {
    assert_erl!(
        "if javascript {
  pub fn main() { 1 }
}",
        "-module(the_app).\n"
    );
}

#[test]
fn included() {
    assert_erl!(
        "if erlang {
  pub fn main() { 1 }
}",
        "-module(the_app).
-compile(no_auto_import).

-export([main/0]).

-spec main() -> integer().
main() ->
    1.
"
    );
}
