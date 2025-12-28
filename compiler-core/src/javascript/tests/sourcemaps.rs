use crate::assert_source_map;

#[test]
fn sourcemap_function_definition() {
    assert_source_map!(
        "pub fn add_2(x) {
  x + 2
}"
    )
}
