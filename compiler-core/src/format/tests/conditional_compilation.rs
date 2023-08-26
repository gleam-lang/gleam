use crate::assert_format;

#[test]
fn multiple() {
    assert_format!(
        "type X

@target(erlang)
type Y {
  Y
}

@target(javascript)
type Z {
  Z
}
"
    );
}
