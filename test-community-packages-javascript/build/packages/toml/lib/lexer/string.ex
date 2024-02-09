defmodule Toml.Lexer.String do
  @moduledoc false

  # This module manages the complexity of lexing quoted and literal
  # strings as defined by the TOML spec.

  import Toml.Lexer.Guards

  @type type ::
          :string
          | :multiline_string

  @type skip :: Toml.Lexer.skip()
  @type lines :: Toml.Lexer.lines()
  @type lexer_err :: Toml.Lexer.lexer_err()
  @type result :: {:ok, binary, {type, skip, binary, lines}} | lexer_err

  @spec lex(binary, skip, lines) :: result
  def lex(binary, skip, lines)

  def lex(<<>>, skip, lines),
    do: {:error, :unexpected_eof, skip, lines}

  def lex(<<?\', ?\', ?\', rest::binary>>, skip, lines) do
    {rest, skip, lines} = trim_newline(rest, skip + 3, lines)
    lex_literal(:multi, rest, skip, [], lines)
  end

  def lex(<<?\', ?\', rest::binary>>, skip, lines),
    do: {:ok, rest, {:string, skip + 2, <<>>, lines}}

  def lex(<<?\', rest::binary>>, skip, lines),
    do: lex_literal(:single, rest, skip + 1, [], lines)

  def lex(<<?\", ?\", ?\", rest::binary>>, skip, lines) do
    {rest, skip, lines} = trim_newline(rest, skip + 3, lines)
    lex_quoted(:multi, rest, skip, [], lines)
  end

  def lex(<<?\", ?\", rest::binary>>, skip, lines),
    do: {:ok, rest, {:string, skip + 2, <<>>, lines}}

  def lex(<<?\", rest::binary>>, skip, lines),
    do: lex_quoted(:single, rest, skip + 1, [], lines)

  defp lex_literal(_type, <<>>, skip, _acc, lines),
    do: {:error, :unclosed_quote, skip, lines}

  # Disallow newlines in single-line literals
  defp lex_literal(:single, <<?\r, ?\n, _::binary>>, skip, _acc, lines),
    do: {:error, :unexpected_newline, skip + 1, lines}

  defp lex_literal(:single, <<?\n, _::binary>>, skip, _acc, lines),
    do: {:error, :unexpected_newline, skip + 1, lines}

  defp lex_literal(type, <<?\r, ?\n, rest::binary>>, skip, acc, lines),
    do: lex_literal(type, rest, skip + 2, [?\n | acc], lines + 1)

  defp lex_literal(type, <<?\n, rest::binary>>, skip, acc, lines),
    do: lex_literal(type, rest, skip + 1, [?\n | acc], lines + 1)

  # Closing quotes
  defp lex_literal(:single, <<?\', rest::binary>>, skip, acc, lines) do
    bin = acc |> Enum.reverse() |> IO.chardata_to_string()
    {:ok, rest, {:string, skip + 1, bin, lines}}
  end

  defp lex_literal(:multi, <<?\', ?\', ?\', rest::binary>>, skip, acc, lines) do
    bin = acc |> Enum.reverse() |> IO.chardata_to_string()
    {:ok, rest, {:multiline_string, skip + 3, bin, lines}}
  end

  # Disallow any control chars in single quoted literals
  defp lex_literal(:single, <<c::utf8, _::binary>>, skip, _acc, lines)
       when (c >= 0 and c <= 31) or c == 127,
       do: {:error, {:invalid_control_char, <<c::utf8>>}, skip + 1, lines}

  # Disallow any control chars in single quoted literals, EXCEPT \t in multi-line literals
  defp lex_literal(:multi, <<?\t, rest::binary>>, skip, acc, lines),
    do: lex_literal(:multi, rest, skip + 1, [?\t | acc], lines)

  defp lex_literal(:multi, <<c::utf8, _::binary>>, skip, _acc, lines)
       when (c >= 0 and c <= 31) or c == 127,
       do: {:error, {:invalid_control_char, <<c::utf8>>}, skip + 1, lines}

  # Eat next character in string
  defp lex_literal(type, <<c::utf8, rest::binary>>, skip, acc, lines),
    do: lex_literal(type, rest, skip + 1, [c | acc], lines)

  # Invalid byte sequence (i.e. invalid unicode)
  defp lex_literal(_type, <<c, _::binary>>, skip, _acc, lines),
    do: {:error, {:invalid_unicode, <<c>>}, skip + 1, lines}

  defp lex_quoted(_type, <<>>, skip, _acc, lines),
    do: {:error, :unclosed_quote, skip, lines}

  # Disallow newlines in single-line strings
  defp lex_quoted(:single, <<?\r, ?\n, _::binary>>, skip, _acc, lines),
    do: {:error, :unexpected_newline, skip + 1, lines}

  defp lex_quoted(:single, <<?\n, _::binary>>, skip, _acc, lines),
    do: {:error, :unexpected_newline, skip + 1, lines}

  defp lex_quoted(type, <<?\r, ?\n, rest::binary>>, skip, acc, lines),
    do: lex_quoted(type, rest, skip + 2, [?\n | acc], lines + 1)

  defp lex_quoted(type, <<?\n, rest::binary>>, skip, acc, lines),
    do: lex_quoted(type, rest, skip + 1, [?\n | acc], lines + 1)

  # Allow escaping newlines in multi-ine strings
  defp lex_quoted(:multi, <<?\\, ?\r, ?\n, rest::binary>>, skip, acc, lines) do
    {rest, skip, lines} = trim_whitespace(rest, skip + 3, lines + 1)
    lex_quoted(:multi, rest, skip, acc, lines)
  end

  defp lex_quoted(:multi, <<?\\, ?\n, rest::binary>>, skip, acc, lines) do
    {rest, skip, lines} = trim_whitespace(rest, skip + 2, lines + 1)
    lex_quoted(:multi, rest, skip, acc, lines)
  end

  # Trim trailing whitespace at end of line
  defp lex_quoted(:multi, <<?\\, ?\s, rest::binary>>, skip, acc, lines) do
    lex_quoted(:multi, <<?\\, rest::binary>>, skip, acc, lines)
  end

  # Allowed escapes
  defp lex_quoted(type, <<?\\, ?\\, rest::binary>>, skip, acc, lines) do
    lex_quoted(type, rest, skip + 2, [?\\ | acc], lines)
  end

  defp lex_quoted(type, <<?\\, ?b, rest::binary>>, skip, acc, lines) do
    lex_quoted(type, rest, skip + 2, [?\b | acc], lines)
  end

  defp lex_quoted(type, <<?\\, ?d, rest::binary>>, skip, acc, lines) do
    lex_quoted(type, rest, skip + 2, [?\d | acc], lines)
  end

  defp lex_quoted(type, <<?\\, ?f, rest::binary>>, skip, acc, lines) do
    lex_quoted(type, rest, skip + 2, [?\f | acc], lines)
  end

  defp lex_quoted(type, <<?\\, ?n, rest::binary>>, skip, acc, lines) do
    lex_quoted(type, rest, skip + 2, [?\n | acc], lines)
  end

  defp lex_quoted(type, <<?\\, ?r, rest::binary>>, skip, acc, lines) do
    lex_quoted(type, rest, skip + 2, [?\r | acc], lines)
  end

  defp lex_quoted(type, <<?\\, ?t, rest::binary>>, skip, acc, lines) do
    lex_quoted(type, rest, skip + 2, [?\t | acc], lines)
  end

  defp lex_quoted(type, <<?\\, ?\", rest::binary>>, skip, acc, lines),
    do: lex_quoted(type, rest, skip + 2, [?\" | acc], lines)

  defp lex_quoted(type, <<?\\, ?u, rest::binary>>, skip, acc, lines) do
    {char, rest, skip2} = unescape_unicode(?u, rest)
    lex_quoted(type, rest, 2 + skip + skip2, [char | acc], lines)
  catch
    :throw, {:invalid_unicode, _} = reason ->
      {:error, reason, skip + 1, lines}
  end

  defp lex_quoted(type, <<?\\, ?U, rest::binary>>, skip, acc, lines) do
    {char, rest, skip2} = unescape_unicode(?U, rest)
    lex_quoted(type, rest, 2 + skip + skip2, [char | acc], lines)
  catch
    :throw, {:invalid_unicode, _} = reason ->
      {:error, reason, skip + 1, lines}
  end

  # Bad escape
  defp lex_quoted(_type, <<?\\, char::utf8, _::binary>>, skip, _acc, lines) do
    {:error, {:invalid_escape, <<?\\, char::utf8>>}, skip + 1, lines}
  end

  # Closing quotes
  defp lex_quoted(:multi, <<?\", ?\", ?\", rest::binary>>, skip, acc, lines) do
    bin = acc |> Enum.reverse() |> IO.chardata_to_string()
    {:ok, rest, {:multiline_string, skip + 3, bin, lines}}
  end

  defp lex_quoted(:single, <<?\", rest::binary>>, skip, acc, lines) do
    bin = acc |> Enum.reverse() |> IO.chardata_to_string()
    {:ok, rest, {:string, skip + 1, bin, lines}}
  end

  # Disallow any unescaped control chars in quoted strings
  defp lex_quoted(_type, <<c::utf8, _::binary>>, skip, _acc, lines)
       when (c >= 0 and c <= 31) or c == 127,
       do: {:error, {:invalid_control_char, <<c::utf8>>}, skip + 1, lines}

  # Eat next character in string
  defp lex_quoted(type, <<c::utf8, rest::binary>>, skip, acc, lines) do
    lex_quoted(type, rest, skip + 1, [c | acc], lines)
  end

  defp lex_quoted(_type, <<c, _::binary>>, skip, _acc, lines),
    do: {:error, {:invalid_unicode, <<c>>}, skip + 1, lines}

  defp trim_newline(<<?\r, ?\n, rest::binary>>, skip, lines), do: {rest, skip + 2, lines + 1}
  defp trim_newline(<<?\n, rest::binary>>, skip, lines), do: {rest, skip + 1, lines + 1}
  defp trim_newline(rest, skip, lines), do: {rest, skip, lines}

  # Trims whitespace (tab, space) up until next non-whitespace character,
  # or until closing delimiter. Only allowed with mulit-line quoted strings.
  defp trim_whitespace(<<c::utf8, rest::binary>>, skip, lines) when is_whitespace(c),
    do: trim_whitespace(rest, skip + 1, lines)

  defp trim_whitespace(<<?\", ?\", ?\", _::binary>> = rest, skip, lines),
    do: {rest, skip, lines}

  defp trim_whitespace(rest, skip, lines),
    do: {rest, skip, lines}

  def unescape_unicode(?U, <<n::8-binary, bin::binary>>) do
    char =
      case :erlang.binary_to_integer(n, 16) do
        u when u <= 0x007F ->
          <<u>>

        u when 0x0080 <= u and u <= 0x07FF ->
          u1 = div(u, 64) + 192
          u2 = mod(u, 64) + 128
          <<u1, u2>>

        u when (0x0080 <= u and u <= 0xD7FF) or (0xE000 <= u and u <= 0xFFFF) ->
          u1 = div(u, 4096) + 224
          u2 = div(mod(u, 4096), 64) + 128
          u3 = mod(u, 64) + 128
          <<u1, u2, u3>>

        u ->
          u1 = div(u, 262_144) + 240
          u2 = div(mod(u, 262_144), 4096) + 128
          u3 = div(mod(u, 4096), 64) + 128
          u4 = mod(u, 64) + 128
          <<u1, u2, u3, u4>>
      end

    # Check that we have a valid utf8 encoding
    case char do
      <<_::utf8>> ->
        {char, bin, 8}

      _ ->
        bad_unicode!(char)
    end
  end

  def unescape_unicode(?u, <<n::4-binary, bin::binary>>) do
    case :erlang.binary_to_integer(n, 16) do
      high when 0xD800 <= high and high <= 0xDBFF ->
        # surrogate pair
        case bin do
          <<?\\, ?u, n2::4-binary, bin2::binary>> ->
            case :erlang.binary_to_integer(n2, 16) do
              low when 0xDC00 <= low and low <= 0xDFFF ->
                <<u::utf16>> = <<high::16, low::16>>
                {<<u::utf8>>, bin2, 4 + 6}

              _ ->
                bad_unicode!(n)
            end

          _ ->
            bad_unicode!(n)
        end

      # second part of surrogate pair (without first part)
      u when (0xDC00 <= u and u <= 0xDFFF) or u < 0 ->
        bad_unicode!(n)

      u ->
        {<<u::utf8>>, bin, 4}
    end
  catch
    :error, :badarg ->
      bad_unicode!(n)
  end

  def unescape_unicode(_type, <<c, _::binary>>) do
    bad_unicode!(<<c>>)
  end

  @spec bad_unicode!(binary) :: no_return
  defp bad_unicode!(byte), do: throw({:invalid_unicode, byte})

  defp mod(x, y) when x > 0, do: rem(x, y)
  defp mod(x, y) when x < 0, do: y + rem(x, y)
  defp mod(0, _), do: 0
end
