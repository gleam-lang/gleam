defmodule Toml.Builder do
  @moduledoc false

  @compile inline: [key: 3, keys: 2, comment: 2, open: 2, close: 1, to_result: 1]

  # Builds a Document, performing validation along the way

  alias Toml.Document

  @doc """
  Creates a new empty TOML document to build with.
  """
  defdelegate new(opts), to: Document

  @doc """
  Starts a new table and sets the context for subsequent key/values
  """
  def push_table(%Document{} = doc, keypath) do
    with {:ok, doc} = push_key(%Document{doc | open_table: nil}, keypath, {:table_decl, %{}}) do
      # We're explicitly opening a new table
      doc |> open(keypath) |> to_result()
    end
  end

  @doc """
  Starts a new array of tables and sets the context for subsequent key/values
  """
  def push_table_array(%Document{} = doc, keypath) do
    with {:ok, doc} = push_key(%Document{doc | open_table: nil}, keypath, {:table_array, []}) do
      # We're explicitly opening a new table
      doc |> open(keypath) |> to_result()
    end
  end

  @doc """
  Push a comment on a stack containing lines of comments applying to some element.
  Comments are collected and assigned to key paths when a key is set, table created, etc.
  """
  def push_comment(%Document{comment_stack: cs} = doc, comment) do
    %Document{doc | comment_stack: [comment | cs]}
  end

  @doc """
  Push a value for a key into the TOML document.

  This operation is used when any key/value pair is set, and table or table array is defined.

  Based on the key the type of the value provided, the behavior of this function varies, as validation
  as performed as part of setting the key, to ensure that redefining keys is prohibited, but that setting
  child keys of existing tables is allowed. Table arrays considerably complicate this unfortunately.
  """
  def push_key(%Document{keys: ks} = doc, [key], value)
      when is_map(value) and map_size(value) == 0
      when is_tuple(value) and elem(value, 0) == :table_decl do
    # New table
    keypath = [key]

    case Map.get(ks, key) do
      nil ->
        doc
        |> key(key, %{})
        |> comment(keypath)
        |> open(keypath)
        |> to_result()

      exists when is_map(exists) ->
        cond do
          map_size(exists) == 0 ->
            # Redefinition
            key_exists!(keypath)

          Enum.all?(exists, fn
            {_, v} when is_map(v) -> true
            _ -> false
          end) ->
            # All keys are sub-tables, we'll allow this
            doc
            |> comment(keypath)
            |> open(keypath)
            |> to_result()

          :else ->
            {k, _} = Enum.find(exists, fn {_, v} -> not is_map(v) end)
            key_exists!(keypath ++ [k])
        end

      _exists ->
        key_exists!(keypath)
    end
  end

  def push_key(%Document{keys: ks} = doc, [key], value) when is_map(value) do
    # Pushing an inline table
    keypath =
      case doc.open_table do
        nil ->
          [key]

        opened ->
          opened ++ [key]
      end

    case push_key_into_table(ks, keypath, value) do
      {:ok, ks} ->
        doc
        |> keys(ks)
        |> comment(keypath)
        |> to_result()

      {:error, :key_exists} ->
        key_exists!(keypath)
    end
  end

  def push_key(%Document{keys: ks} = doc, [key], {:table_array, _} = value) do
    keypath = [key]

    case push_key_into_table(ks, [key], value) do
      {:ok, ks} ->
        doc
        |> keys(ks)
        |> comment(keypath)
        |> close()
        |> to_result()

      {:error, :key_exists} ->
        key_exists!(keypath)
    end
  end

  def push_key(%Document{keys: ks, open_table: nil} = doc, [key], value) do
    # Pushing a key/value pair at the root level of the document
    keypath = [key]

    if Map.has_key?(ks, key) do
      key_exists!(keypath)
    end

    doc
    |> key(key, value)
    |> comment(keypath)
    |> close()
    |> to_result()
  end

  def push_key(%Document{keys: ks, open_table: opened} = doc, [key], value) do
    # Pushing a key/value pair when a table is open
    keypath = opened ++ [key]

    case push_key_into_table(ks, keypath, value) do
      {:ok, ks} ->
        doc
        |> keys(ks)
        |> comment(keypath)
        |> to_result()

      {:error, :key_exists} ->
        key_exists!(keypath)
    end
  end

  def push_key(%Document{keys: ks} = doc, keypath, value)
      when is_list(keypath) and is_map(value)
      when is_list(keypath) and is_tuple(value) and elem(value, 0) == :table_decl do
    # Pushing a multi-part key with a table value
    keypath =
      case doc.open_table do
        nil ->
          keypath

        opened ->
          opened ++ keypath
      end

    case push_key_into_table(ks, keypath, value) do
      {:ok, ks} ->
        doc
        |> keys(ks)
        |> comment(keypath)
        |> to_result()

      {:error, :key_exists} ->
        key_exists!(keypath)
    end
  end

  def push_key(%Document{keys: ks} = doc, keypath, value) when is_list(keypath) do
    # Pushing a multi-part key with a plain value
    keypath =
      case doc.open_table do
        nil ->
          keypath

        opened ->
          opened ++ keypath
      end

    case push_key_into_table(ks, keypath, value) do
      {:ok, ks} ->
        doc
        |> keys(ks)
        |> comment(keypath)
        |> to_result()

      {:error, :key_exists} ->
        key_exists!(keypath)
    end
  end

  # Handles pushing a value into a map, arbitrarily deep, and respecting
  # the desired behavior of table arrays. Can be used with any map.
  #
  # NOTE: This is similar to `put_in/3`, but does not raise if a key does
  # exist, instead it creates a new table (map) at the level needed, and continues
  # pushing the key into this new table
  @doc false
  def push_key_into_table({:table_array, array}, [key], {:table_array, _} = value) do
    case array do
      [] ->
        {:ok, {:table_array, [Map.put(%{}, key, value)]}}

      [h | t] when is_map(h) ->
        case Map.get(h, key) do
          nil ->
            {:ok, {:table_array, [Map.put(h, key, value) | t]}}

          {:table_array, items} ->
            {:ok, {:table_array, [Map.put(h, key, {:table_array, [%{} | items]}) | t]}}

          _ ->
            {:error, :key_exists}
        end
    end
  end

  def push_key_into_table({:table_array, array}, keypath, {:table_decl, value}) do
    case array do
      [] ->
        {:ok, {:table_array, [value]}}

      [h | t] when is_map(h) ->
        case push_key_into_table(h, keypath, {:table_decl, value}) do
          {:ok, h2} ->
            {:ok, {:table_array, [h2 | t]}}

          {:error, _} = err ->
            err
        end
    end
  end

  def push_key_into_table({:table_array, array}, keypath, value) when is_map(value) do
    case array do
      [] ->
        {:ok, {:table_array, [value]}}

      [h | t] when is_map(h) ->
        case push_key_into_table(h, keypath, value) do
          {:ok, h2} ->
            {:ok, {:table_array, [h2 | t]}}

          {:error, _} = err ->
            err
        end
    end
  end

  def push_key_into_table({:table_array, array}, keypath, value) do
    # Adding key/value to last table item
    case array do
      [] ->
        case push_key_into_table(%{}, keypath, value) do
          {:ok, item} ->
            {:ok, {:table_array, [item]}}

          {:error, _} = err ->
            err
        end

      [h | t] when is_map(h) ->
        case push_key_into_table(h, keypath, value) do
          {:ok, h2} ->
            {:ok, {:table_array, [h2 | t]}}

          {:error, _} = err ->
            err
        end
    end
  end

  def push_key_into_table(ts, [key], {:table_decl, value}) do
    case Map.get(ts, key) do
      nil ->
        {:ok, Map.put(ts, key, value)}

      exists when is_map(exists) ->
        # We're reopening a table, but we allow it as long as keys
        # in that table don't override previously defined keys
        {:ok, ts}

      _exists ->
        {:error, :key_exists}
    end
  end

  def push_key_into_table(ts, [key], value) when is_map(ts) do
    # Reached final table
    case Map.get(ts, key) do
      nil ->
        {:ok, Map.put(ts, key, value)}

      {:table_array, items}
      when is_list(items) and is_tuple(value) and elem(value, 0) == :table_array ->
        # Appending to table array
        {:ok, Map.put(ts, key, {:table_array, [%{} | items]})}

      {:table_array, items} when is_list(items) and is_map(value) ->
        # Pushing into table array
        {:ok, Map.put(ts, key, {:table_array, [value | items]})}

      exists when is_map(exists) and is_map(value) and map_size(value) > 0 ->
        result =
          Enum.reduce(value, exists, fn {k, v}, acc ->
            case push_key_into_table(acc, [k], v) do
              {:ok, acc2} ->
                acc2

              {:error, _} = err ->
                throw(err)
            end
          end)

        {:ok, result}

      _exists ->
        {:error, :key_exists}
    end
  catch
    :throw, {:error, _} = err ->
      err
  end

  def push_key_into_table(ts, _keypath, _value) when not is_map(ts) do
    # The table we're trying to push into is not itself a table, this is redefinition!
    {:error, :key_exists}
  end

  def push_key_into_table(ts, [table | keypath], value) when is_map(ts) do
    result =
      case Map.get(ts, table) do
        nil ->
          push_key_into_table(%{}, keypath, value)

        child ->
          push_key_into_table(child, keypath, value)
      end

    case result do
      {:ok, child} ->
        {:ok, Map.put(ts, table, child)}

      {:error, _} = err ->
        err
    end
  end

  # Raised if the given keypath has already been defined
  # and the value being set at that keypath is not an inline
  # table (effectively extending the keypath and pushing multiple values),
  # or table array (resulting in a new table being pushed into that array)
  @spec key_exists!([binary]) :: no_return
  defp key_exists!(keypath),
    do: error!({:key_exists, Enum.join(keypath, ".")})

  # Raised due to any other document builder error
  @spec error!([binary]) :: no_return
  defp error!(reason),
    do: throw({:error, {:invalid_toml, reason}})

  # Set a key at the document root
  defp key(%Document{keys: keys} = doc, key, value),
    do: %Document{doc | keys: Map.put(keys, key, value)}

  # Replace the set of keys in the document
  defp keys(%Document{} = doc, keys),
    do: %Document{doc | keys: keys}

  # Comment the given key, if comments are available on the stack
  defp comment(%Document{comments: cs, comment_stack: stack} = doc, key) do
    comment =
      stack
      |> Enum.reverse()
      |> Enum.join("\n")

    if byte_size(comment) > 0 do
      %Document{doc | comment_stack: [], comments: Map.put(cs, key, comment)}
    else
      %Document{doc | comment_stack: []}
    end
  end

  # Open a table at the given key
  defp open(%Document{} = doc, key),
    do: %Document{doc | open_table: key}

  # Close any open table
  defp close(%Document{} = doc),
    do: %Document{doc | open_table: nil}

  # Wrap the document in a ok tuple
  defp to_result(%Document{} = doc),
    do: {:ok, doc}
end
