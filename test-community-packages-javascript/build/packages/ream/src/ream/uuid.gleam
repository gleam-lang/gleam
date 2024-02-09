//// This module implements the UUID v4 generation algorithm.

import gleam/int
import gleam/string

pub fn new() -> BitString {
  let <<u0:size(48), _:size(4), u1:size(12), _:size(2), u2:size(62)>> =
    crypto_strong_rand_bytes(16)

  <<u0:size(48), 4:size(4), u1:size(12), 2:size(2), u2:size(62)>>
}

pub fn to_string(uuid: BitString) -> String {
  parts(uuid)
  |> string.join(with: "-")
}

pub fn to_int(uuid: BitString) -> Int {
  let <<uuid_int:128>> = uuid
  uuid_int
}

pub fn from_int(uuid: Int) -> BitString {
  <<uuid:128>>
}

pub fn from_string(uuid: String) -> BitString {
  let assert Ok(uuid_int) =
    uuid
    |> string.replace(each: "-", with: "")
    |> int.base_parse(16)

  from_int(uuid_int)
}

pub fn parts(uuid: BitString) -> List(String) {
  let <<p1:32, p2:16, p3:16, p4:16, p5:48>> = uuid
  [to_hex(p1, 8), to_hex(p2, 4), to_hex(p3, 4), to_hex(p4, 4), to_hex(p5, 12)]
}

fn to_hex(n: Int, size: Int) -> String {
  int.to_base16(n)
  |> string.lowercase()
  |> string.pad_left(to: size, with: "0")
}

external fn crypto_strong_rand_bytes(Int) -> BitString =
  "crypto" "strong_rand_bytes"
