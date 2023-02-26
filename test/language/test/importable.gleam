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
