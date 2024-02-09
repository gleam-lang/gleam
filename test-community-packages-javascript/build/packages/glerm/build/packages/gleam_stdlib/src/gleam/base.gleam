import gleam/bit_array

@deprecated("Please use `base64_encode` in the `gleam/bit_array` module instead.")
pub fn encode64(input: BitArray, padding: Bool) -> String {
  bit_array.base64_encode(input, padding)
}

@deprecated("Please use `base64_decode` in the `gleam/bit_array` module instead.")
pub fn decode64(encoded: String) -> Result(BitArray, Nil) {
  bit_array.base64_decode(encoded)
}

@deprecated("Please use `base64_url_encode` in the `gleam/bit_array` module instead.")
pub fn url_encode64(input: BitArray, padding: Bool) -> String {
  bit_array.base64_url_encode(input, padding)
}

@deprecated("Please use `base64_url_decode` in the `gleam/bit_array` module instead.")
pub fn url_decode64(encoded: String) -> Result(BitArray, Nil) {
  bit_array.base64_url_decode(encoded)
}
