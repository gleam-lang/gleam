use crate::assert_erl;

#[test]
fn build_in_erlang_type_escaping() {
    assert_erl!("pub external type Map");
}

#[test]
fn escape_erlang_reserved_keywords_in_type_names() {
    // list of all reserved words in erlang
    // http://erlang.org/documentation/doc-5.8/doc/reference_manual/introduction.html
    assert_erl!(
        r#"pub type After { TestAfter }
pub type And { TestAnd }
pub type Andalso { TestAndAlso }
pub type Band { TestBAnd }
pub type Begin { TestBegin }
pub type Bnot { TestBNot }
pub type Bor { TestBOr }
pub type Bsl { TestBsl }
pub type Bsr { TestBsr }
pub type Bxor { TestBXor }
pub type Case { TestCase }
pub type Catch { TestCatch }
pub type Cond { TestCond }
pub type Div { TestDiv }
pub type End { TestEnd }
pub type Fun { TestFun }
pub type If { TestIf }
pub type Let { TestLet }
pub type Not { TestNot }
pub type Of { TestOf }
pub type Or { TestOr }
pub type Orelse { TestOrElse }
pub type Query { TestQuery }
pub type Receive { TestReceive }
pub type Rem { TestRem }
pub type Try { TestTry }
pub type When { TestWhen }
pub type Xor { TestXor }"#
    );
}
