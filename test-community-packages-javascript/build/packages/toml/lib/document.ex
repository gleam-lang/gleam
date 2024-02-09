defmodule Toml.Document do
  @moduledoc false

  # Represents a TOML document, and handles conversion to a plain map
  # See `Toml.Builder` for the actual logic for constructing the document.

  defstruct [:keys, :comments, :open_table, :comment_stack, :keyfun, :transforms]

  # A key is either binary or atom depending on the decoder option value
  @type key :: binary | atom | term

  # A value is the fully decoded value from the TOML
  @type value ::
          %{key => value}
          | {:table_array, [%{key => value}]}
          | number
          | binary
          | NaiveDateTime.t()
          | DateTime.t()
          | Date.t()
          | Time.t()
          | [value]

  # A keypath is a list of keys, they are all of the same key type
  @type keypath :: list(binary) | list(atom) | list(term)

  @type t :: %__MODULE__{
          keys: %{key => value},
          comments: %{keypath => binary},
          open_table: keypath | nil,
          comment_stack: [binary],
          keyfun: nil | (binary -> term | no_return),
          transforms: [Toml.Transform.t()]
        }

  @doc """
  Create a new empty TOML document
  """
  @spec new(Toml.opts()) :: t
  def new(opts) when is_list(opts) do
    keyfun = to_key_fun(Keyword.get(opts, :keys, :strings))
    transforms = Keyword.get(opts, :transforms, [])

    %__MODULE__{
      keys: %{},
      comments: %{},
      open_table: nil,
      comment_stack: [],
      keyfun: keyfun,
      transforms: transforms
    }
  end

  @doc """
  Convert the given TOML document to a plain map.

  During conversion to a plain map, keys are converted according 
  to the key type defined when the document was created.

  In addition to converting keys, if transforms were defined, they are
  applied to values depth-first, bottom-up. Transforms are first composed
  into a single function, designed to be executed in the order they appear 
  in the list provided; if any transform returns an error, conversion is
  stopped and an error is returned - otherwise, the value is passed from
  transformer to transformer and the final result replaces the value in the
  document.
  """
  def to_map(%__MODULE__{keys: keys, keyfun: keyfun, transforms: ts}) do
    transform =
      case ts do
        [] ->
          nil

        ts when is_list(ts) ->
          Toml.Transform.compose(ts)
      end

    {:ok, to_map2(keys, keyfun, transform)}
  catch
    _, {:error, {:keys, {:non_existing_atom, _}}} = err ->
      err
  end

  # Called when a table is being converted
  defp to_map2(m, nil, nil) when is_map(m) do
    for {k, v} <- m, into: %{}, do: {k, to_map3(k, v, nil, nil)}
  end

  defp to_map2(m, keyfun, nil) when is_map(m) and is_function(keyfun, 1) do
    for {k, v} <- m, into: %{} do
      k2 = keyfun.(k)
      {k2, to_map3(k2, v, keyfun, nil)}
    end
  end

  defp to_map2(m, nil, transform) when is_map(m) and is_function(transform, 2) do
    for {k, v} <- m, into: %{} do
      v2 = to_map3(k, v, nil, transform)
      {k, v2}
    end
  end

  defp to_map2(m, keyfun, transform)
       when is_map(m) and is_function(keyfun, 1) and is_function(transform, 2) do
    for {k, v} <- m, into: %{} do
      k2 = keyfun.(k)
      v2 = to_map3(k2, v, keyfun, transform)
      {k2, v2}
    end
  end

  # Called when a table value is being converted
  defp to_map3(_key, %_{} = s, _keyfun, nil), do: s
  defp to_map3(key, %_{} = s, _keyfun, transform), do: transform.(key, s)

  defp to_map3(key, list, keyfun, nil) when is_list(list) do
    for v <- list, do: to_map3(key, v, keyfun, nil)
  end

  defp to_map3(key, list, _keyfun, transform) when is_list(list) do
    transform.(key, list)
  end

  defp to_map3(_key, {:table_array, list}, keyfun, nil) do
    for v <- Enum.reverse(list) do
      to_map2(v, keyfun, nil)
    end
  end

  defp to_map3(key, {:table_array, list}, keyfun, transform) do
    for v <- Enum.reverse(list) do
      to_map2(v, keyfun, transform)
    end
    |> (&transform.(key, &1)).()
  end

  defp to_map3(_key, v, keyfun, nil) when is_map(v) do
    to_map2(v, keyfun, nil)
  end

  defp to_map3(key, v, keyfun, transform) when is_map(v) and is_function(transform) do
    transform.(key, to_map2(v, keyfun, transform))
  end

  defp to_map3(_key, v, _keyfun, nil), do: v
  defp to_map3(key, v, _keyfun, transform), do: transform.(key, v)

  # Convert the value of `:keys` to a key conversion function (if not already one)
  @valid_keys_opts [:atoms, :atoms!, :strings, "(key :: String.t) -> term"]
  defp to_key_fun(:atoms), do: &to_atom/1
  defp to_key_fun(:atoms!), do: &to_existing_atom/1
  defp to_key_fun(:strings), do: nil
  defp to_key_fun(fun) when is_function(fun, 1), do: fun
  defp to_key_fun(invalid), do: throw({:badarg, {:keys, invalid, @valid_keys_opts}})

  # Convert the given key (as binary) to an atom
  # Handle converting uppercase keys to module names rather than plain atoms
  defp to_atom(<<c::utf8, _::binary>> = key) when c >= ?A and c <= ?Z do
    Module.concat([key])
  end

  defp to_atom(key), do: String.to_atom(key)

  # Convert the given key (as binary) to an existing atom
  # Handle converting uppercase keys to module names rather than plain atoms
  #
  # NOTE: This throws an error if the atom does not exist, and is intended to
  # be handled in the decoder
  defp to_existing_atom(<<c::utf8, _::binary>> = key) when c >= ?A and c <= ?Z do
    Module.concat([String.to_existing_atom(key)])
  rescue
    _ ->
      throw({:error, {:keys, {:non_existing_atom, key}}})
  end

  defp to_existing_atom(key) do
    String.to_existing_atom(key)
  rescue
    _ ->
      throw({:error, {:keys, {:non_existing_atom, key}}})
  end
end
