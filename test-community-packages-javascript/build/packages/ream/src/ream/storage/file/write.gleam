import gleam/erlang/file

pub type Result {
  Ok
  Error(reason: file.Reason)
}
