defmodule Toml.Lexer.Guards do
  @moduledoc false

  defguard is_lowercase(c) when c >= ?a and c <= ?z

  defguard is_uppercase(c) when c >= ?A and c <= ?Z

  defguard is_alpha(c) when is_lowercase(c) or is_uppercase(c)

  defguard is_digit(c) when c >= ?0 and c <= ?9

  defguard is_hex(c) when is_digit(c) or (c >= ?a and c <= ?z) or (c >= ?A and c <= ?Z)

  defguard is_octal(c) when c >= ?0 and c <= ?8

  defguard is_bin(c) when c == ?0 or c == ?1

  defguard is_whitespace(c) when c == ?\s or c == ?\t

  defguard is_quote(c) when c == ?\" or c == ?\'
end
