import gleam/erlang/file

pub type Result {
  Ok(data: BitString)
  Eof
  Error(reason: file.Reason)
}
