import should

external fn to_graphemes(String) -> List(List(Int))
 = "string" "to_graphemes"

pub fn unicode_overflow_test() {
    // In erlang, literally creating binaries can cause entries to overflow.
    // For example `<<"🌵">> == <<"5">>` evaluates to true.
    // This checks that we are not doing that.
    // See: https://github.com/gleam-lang/gleam/issues/457
    "🌵"
    |> should.not_equal(_, "5")

    "🤷‍♂️"
    |> to_graphemes
    |> should.equal(_, [[129335,8205,9794,65039]])
}
