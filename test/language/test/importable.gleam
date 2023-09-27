pub type NoFields {
  NoFields
}

/// This function has argument names that are not valid in Erlang or JavaScript
pub fn bad_argument_names(in, class, receive) {
  #(in, class, receive)
}

/// This custom type has label names that are not valid in Erlang or JavaScript
pub type BadLabelNames {
  BadLabelNames(in: String, class: String, receive: String)
}

pub const ints_in_bit_string = <<1, 2, 3>>

pub const string_in_bit_string = <<"Gleam":utf8>>

pub const data = <<
  0x1,
  2,
  2:size(16),
  0x4:size(32),
  "Gleam":utf8,
  4.2:float,
  <<<<1, 2, 3>>:bit_string, "Gleam":utf8, 1024>>:bit_string,
>>

pub fn get_bit_string() {
  <<
    0x1,
    2,
    2:size(16),
    0x4:size(32),
    "Gleam":utf8,
    4.2:float,
    <<<<1, 2, 3>>:bit_string, "Gleam":utf8, 1024>>:bit_string,
  >>
}

pub const language = "gleam"

pub type Movie {
  Movie(title: String)
}

pub const war_games = Movie("WarGames")
