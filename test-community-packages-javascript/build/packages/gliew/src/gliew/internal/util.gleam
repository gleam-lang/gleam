import gleam/int
import gleam/string
import gleam/crypto

pub fn random_hex_string(len: Int) {
  crypto.strong_random_bytes(len)
  |> to_hex_string
}

fn to_hex_string(bstr: BitString) {
  case bstr {
    <<>> -> ""
    <<a:8, rest:bit_string>> -> {
      int.to_base16(a)
      |> string.lowercase <> to_hex_string(rest)
    }
  }
}
