use crate::assert_erl;

#[test]
fn named_args_count_once() {
    assert_erl!(
        "
        pub fn foo() -> Result(a, a) {
            todo
        }
        "
    );
}

#[test]
fn nested_named_args_count_once() {
    assert_erl!(
        "
        pub type Nested(a, b) {
            VariantX(field_a: a, field_b: b)
            VariantY(field_a: a, field_b: b)
        }

        pub type Parent(a, b) {
            ParentX(Nested(a,b))
            ParentY(Nested(a,b))
        }

        pub fn foo1() -> Parent(a, a) {
            todo
        }

        pub fn foo2() -> Parent(a, b) {
            todo
        }

        pub fn foo3(val_a: a) -> Parent(a, a) {
            todo
        }

        pub fn foo4(val_a: a, val_b: b) -> Parent(a, b) {
            todo
        }
        "
    );
}
