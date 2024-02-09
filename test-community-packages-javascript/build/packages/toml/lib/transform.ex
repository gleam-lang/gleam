defmodule Toml.Transform do
  @moduledoc """
  Defines the behavior for custom transformations of decoded TOML values.

  See the documentation for `c:transform/2` for more details.
  """

  @type t :: module
  @type key :: binary | atom | term
  @type keypath :: [binary] | [atom] | [term]
  @type value ::
          %{key => value}
          | [value]
          | number
          | binary
          | NaiveDateTime.t()
          | DateTime.t()
          | Date.t()
          | Time.t()

  def __using__(_) do
    quote do
      @behaviour unquote(__MODULE__)
    end
  end

  @doc """
  This function is invoked for every key/value pair in the document, in a depth-first,
  bottom-up, traversal of the document.

  The transformer must return one of two values:

    * `{:error, term}` - an error occurred in the transformer, and decoding should fail
    * `term` - replace the value for the given key with the value returned from the 
      transformer in the final document
    
  ## Example

  An example transformation would be the conversion of tables of a certain shape to a known struct value.

  The struct:

      defmodule Server do
        defstruct [:name, :ip, :ports]
      end
      

  TOML which contains a table of this shape:

      [servers.alpha]
      ip = "192.168.1.1"
      ports = [8080, 8081]
      
      [servers.beta]
      ip = "192.168.1.2"
      ports = [8082, 8083]
      

  And finally, the transforer implementation:

      defmodule ServerTransform do
        use Toml.Transform

        # Transform IP strings to Erlang address tuples
        def transform(:ip, ip) when is_binary(ip) do
          case :inet.parse_ipv4_address(String.to_charlist(ip)) do
            {:ok, result} ->
              result
            {:error, reason} ->
              {:error, {:invalid_ip, ip, reason}}
          end
        end

        # Non-binary values for IP addresses should return an error
        def transform(:ip, ip), do: {:error, {:invalid_ip, ip, :expected_string}}

        # Transform the server objects to Server structs
        def transform(:servers, servers) when is_map(servers) do
          for {name, server} <- servers do
            struct(Server, Map.put(server, :name, name))
          end
        end

        # Non-map values for servers should return an error
        def transform(:servers, s), do: {:error, {:invalid_server, s}}

        # Ignore all other values
        def transform(_key, v), do: v
      end
      
    Assuming we decode with the following options: 

        Toml.decode!(content, keys: :atoms, transforms: [ServerTransform])
      
    The result would be:

        %{
          servers: [
            %Server{name: :alpha, ip: {192,168,1,1}, ports: [8080, 8081]},
            %Server{name: :beta, ip: {192,168,1,2}, ports: [8082, 8083]}
          ]
         }

  """
  @callback transform(key, value) :: {:error, term} | term

  # Given a list of transform functions, compose them into a single transformation pass
  @doc false
  def compose(mods) when is_list(mods) do
    Enum.reduce(mods, nil, &compose_transforms/2)
  end

  # The first transform in the list does not require composition
  defp compose_transforms(mod, nil), do: &mod.transform/2
  # All subsequent transforms are composed with the previous
  defp compose_transforms(mod, acc) when is_atom(mod) and is_function(acc) do
    fn k, v ->
      case acc.(k, v) do
        {:error, _} = err ->
          throw(err)

        v2 ->
          mod.transform(k, v2)
      end
    end
  end
end
