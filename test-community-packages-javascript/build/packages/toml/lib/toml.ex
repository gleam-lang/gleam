defmodule Toml do
  @external_resource "README.md"

  @moduledoc File.read!(Path.join([__DIR__, "..", "README.md"]))

  @type key :: binary | atom | term
  @type opt ::
          {:keys, :atoms | :atoms! | :string | (key -> term)}
          | {:filename, String.t()}
          | {:transforms, [Toml.Transform.t()]}
  @type opts :: [opt]

  @type reason :: {:invalid_toml, binary} | binary
  @type error :: {:error, reason}

  @doc """
  Decode the given binary as TOML content

  ## Options

  You can pass the following options to configure the decoder behavior:

    * `:filename` - pass a filename to use in error messages
    * `:keys` - controls how keys in the document are decoded. Possible values are:
      * `:strings` (default) - decodes keys as strings
      * `:atoms` - converts keys to atoms with `String.to_atom/1`
      * `:atoms!` - converts keys to atoms with `String.to_existing_atom/1`
      * `(key -> term)` - converts keys using the provided function
    * `:transforms` - a list of custom transformations to apply to decoded TOML values,
      see `c:Toml.Transform.transform/2` for details.
      
  ## Decoding keys to atoms

  The `:atoms` option uses the `String.to_atom/1` call that can create atoms at runtime.
  Since the atoms are not garbage collected, this can pose a DoS attack vector when used
  on user-controlled data. It is recommended that if you either avoid converting to atoms,
  by using `keys: :strings`, or require known keys, by using the `keys: :atoms!` option, 
  which will cause decoding to fail if the key is not an atom already in the atom table.

  ## Transformations

  You should rarely need custom datatype transformations, but in some cases it can be quite
  useful. In particular if you want to transform things like IP addresses from their string
  form to the Erlang address tuples used in most `:inet` APIs, a custom transform can ensure
  that all addresses are usable right away, and that validation of those addresses is done as
  part of decoding the document.

  Keep in mind that transforms add additional work to decoding, which may result in reduced 
  performance, if you don't need the convenience, or the validation, deferring such conversions
  until the values are used may be a better approach, rather than incurring the overhead during decoding.
  """
  @spec decode(binary) :: {:ok, map} | error
  @spec decode(binary, opts) :: {:ok, map} | error
  defdelegate decode(bin, opts \\ []), to: __MODULE__.Decoder

  @doc """
  Same as `decode/1`, but returns the document directly, or raises `Toml.Error` if it fails.
  """
  @spec decode!(binary) :: map | no_return
  @spec decode!(binary, opts) :: map | no_return
  defdelegate decode!(bin, opts \\ []), to: __MODULE__.Decoder

  @doc """
  Decode the file at the given path as TOML

  Takes same options as `decode/2`
  """
  @spec decode_file(binary) :: {:ok, map} | error
  @spec decode_file(binary, opts) :: {:ok, map} | error
  defdelegate decode_file(path, opts \\ []), to: __MODULE__.Decoder

  @doc """
  Same as `decode_file/1`, but returns the document directly, or raises `Toml.Error` if it fails.
  """
  @spec decode_file!(binary) :: map | no_return
  @spec decode_file!(binary, opts) :: map | no_return
  defdelegate decode_file!(path, opts \\ []), to: __MODULE__.Decoder

  @doc """
  Decode the given stream as TOML.

  Takes same options as `decode/2`
  """
  @spec decode_stream(Enumerable.t()) :: {:ok, map} | error
  @spec decode_stream(Enumerable.t(), opts) :: {:ok, map} | error
  defdelegate decode_stream(stream, opts \\ []), to: __MODULE__.Decoder

  @doc """
  Same as `decode_stream/1`, but returns the document directly, or raises `Toml.Error` if it fails.
  """
  @spec decode_stream!(Enumerable.t()) :: map | no_return
  @spec decode_stream!(Enumerable.t(), opts) :: map | no_return
  defdelegate decode_stream!(stream, opts \\ []), to: __MODULE__.Decoder
end
