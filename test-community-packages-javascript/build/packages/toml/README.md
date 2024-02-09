# TOML for Elixir

[![Main](https://github.com/bitwalker/toml-elixir/workflows/elixir/badge.svg?branch=main)](https://github.com/bitwalker/toml-elixir/actions?query=workflow%3A%22elixir%22+branch%3Amain)
[![Hex.pm Version](https://img.shields.io/hexpm/v/toml.svg?style=flat)](https://hex.pm/packages/toml)
[![Hex Docs](https://img.shields.io/badge/hex-docs-lightgreen.svg?style=flat)](https://hexdocs.pm/toml)
[![Total Download](https://img.shields.io/hexpm/dt/toml.svg?style=flat)](https://hex.pm/packages/toml)
[![Last Updated](https://img.shields.io/github/last-commit/bitwalker/toml-elixir.svg?style=flat)](https://github.com/bitwalker/toml-elixir/commits/main)

This is a TOML library for Elixir projects. 

It is compliant with version 1.0 of the [official TOML specification](https://github.com/toml-lang/toml). You can find a
brief overview of the feature set below, but you are encouraged to read the full spec at the link above (it is short and easy to read!).

## Features

- Decode from string, file, or stream
- Fully compliant with the latest version of the TOML spec
- Is tested against [toml-test](https://github.com/BurntSushi/toml-test), a test
  suite for spec-compliant TOML encoders/decoders, used by implementations in
  multiple languages. The test suite has been integrated into this project to be
  run under Mix so that we get better error information and so it can run as
  part of the test suite.
- Decoder produces a map with values using the appropriate Elixir data types for
  representation
- Supports extension via value transformers (see `Toml.Transform` docs for details)
- Supports use as a configuration provider in Distillery 2.x+ (use TOML
  files for configuration!)
- Decoder is written by hand to take advantage of various optimizations.
- Library passes Dialyzer checks

## Comparison To Other Libraries

I compared `toml` to four other libraries:

- `toml_elixir`
- `tomlex`
- `jerry`
- `etoml`

Of these four, none correctly implement the 0.5.0 specification. Either they are
targeting older versions of the spec (in `etoml`, it is built against pre-0.1),
are not fully implemented (i.e. don't support all features), or have bugs which
prevent them from properly parsing a 0.5.0 example file (the
`test/fixtures/example.toml` file in this repository).

If you are looking for a TOML library, at present `toml` is the only one which
full implements the spec and correctly decodes `example.toml`.

## Installation

This library is available on Hex as `:toml`, and can be added to your deps like so:

```elixir
def deps do
  [
    {:toml, "~> 0.7"}
  ]
end
```

NOTE: You can determine the latest version on Hex by running `mix hex.info toml`.

## Type Conversions

In case you are curious how TOML types are translated to Elixir types, the
following table provides the conversions.

**NOTE:** The various possible representations of each type, such as
hex/octal/binary integers, quoted/literal strings, etc., are considered to be
the same base type (e.g. integer and string respectively in the examples given).

| TOML | Elixir |
|-------|-------|
| String | String.t (binary) |
| Integer | integer |
| inf | :infinity |
| +inf | :infinity |
| -inf | :negative_infinity |
| nan | :nan |
| +nan | :nan |
| -nan | :negative_nan |
| Boolean | boolean |
| Offset Date-Time | DateTime.t |
| Local Date-Time | NaiveDateTime.t |
| Local Date | Date.t |
| Local Time | Time.t |
| Array | list |
| Table | map |
| Table Array | list(map) |

## Implementation-specific Behaviors

Certain features of TOML have implementation-specific behavior:

- `-inf`, `inf`, and `+inf` are all valid infinity values in TOML.
  In Erlang/Elixir, these don't have exact representations. Instead, by convention, 
  `:infinity` is used for positive infinity, as atoms are always larger than integers
  when using comparison operators, so `:infinity > <any integer>` will always be true.
  However, negative infinity cannot be represented, as numbers are always considered smaller
  than every other type in term comparisons. Instead, we represent it with `:negative_infinity`,
  so that the type information is not lost, but you must be careful to deal with it specifically
  in comparisons/sorting/etc.
- `-nan`, `nan`, and `+nan` are all valid NaN (not a number) values in TOML. In Erlang/Elixir,
  NaN is traditionally represented with `:nan`, but there is no representation for negative NaN,
  and no API actually produces `:nan`, instead invalid numbers typically raise errors, in the typical 
  spirit of "let it crash" in the face of errors. For purposes of preserving type information though,
  we use the `:nan` convention, and `:negative_nan` for -NaN. You will need to take care to deal with
  these values manually if the values need to be preserved.
- The maximum precision of times in the various time types is microseconds (i.e. precision to six decimal places),
  if you provide higher precision values (i.e. nanoseconds), the extra precision will be lost.
- Hex, octal, and binary numbers are converted to integers, so serializing those values after decoding
  them from a TOML document will be in their decimal representation.

## Example Usage

The following is a brief overview of how to use this library. First, let's take
a look at an example TOML file, as borrowed from the [TOML
homepage](https://github.com/toml-lang/toml):

``` toml
# This is a TOML document.

title = "TOML Example"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00-08:00 # First class dates

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true

[servers]

  # Indentation (tabs and/or spaces) is allowed but not required
  [servers.alpha]
  ip = "10.0.0.1"
  dc = "eqdc10"

  [servers.beta]
  ip = "10.0.0.2"
  dc = "eqdc10"

[clients]
data = [ ["gamma", "delta"], [1, 2] ]

# Line breaks are OK when inside arrays
hosts = [
  "alpha",
  "omega"
]
```

### Parsing

```elixir
iex> input = """
[database]
server = "192.168.1.1"
"""
...> {:ok, %{"database" => %{"server" => "192.168.1.1"}}} = Toml.decode(input)
...> {:ok, %{database: %{server: "192.168.1.1"}}} = Toml.decode(input, keys: :atoms)
...> stream = File.stream!("example.toml")
...> {:ok, %{"database" => %{"server" => "192.168.1.1"}}} = Toml.decode_stream(stream)
...> {:ok, %{"database" => %{"server" => "192.168.1.1"}}} = Toml.decode_file("example.toml")
...> invalid = """
[invalid]
a = 1 b = 2
"""
...> {:error, {:invalid_toml, reason}} = Toml.decode(invalid); IO.puts(reason)
expected '\n', but got 'b' in nofile on line 2:

    a = 1 b = 2
         ^

:ok
```

### Transforms

Support for extending value conversions is provided by the `Toml.Transform`
behavior. An example is shown below:

Given the following TOML document:

``` toml
[servers.alpha]
ip = "192.168.1.1"
ports = [8080, 8081]

[servers.beta]
ip = "192.168.1.2"
ports = [8082, 8083]
```

And the following modules:

``` elixir
defmodule Server do
  defstruct [:name, :ip, :ports]
end

defmodule IPStringToCharlist do
  use Toml.Transform
  
  def transform(:ip, v) when is_binary(v) do
    String.to_charlist(v)
  end
  def transform(_k, v), do: v
end

defmodule CharlistToIP do
  use Toml.Transform
  
  def transform(:ip, v) when is_list(v) do
    case :inet.parse_ipv4_address(v) do
      {:ok, address} ->
        address
      {:error, reason} ->
        {:error, {:invalid_ip_address, reason}}
    end
  end
  def transform(:ip, v), do: {:error, {:invalid_ip_address, v}}
  def transform(_k, v), do: v
end

defmodule ServerMapToList do
  use Toml.Transform
  
  def transform(:servers, v) when is_map(v) do
    for {name, server} <- v, do: struct(Server, Map.put(server, :name, name))
  end
  def transform(_k, v), do: v
end
```

You can convert the TOML document to a more strongly-typed version using the
above transforms like so:

```elixir
iex> transforms = [IPStringToCharlist, CharlistToIP, ServerMapToList]
...> {:ok, result} = Toml.decode("example.toml", keys: :atoms, transforms: transforms)
%{servers: [%Server{name: :alpha, ip: {192,168,1,1}}, ports: [8080, 8081] | _]}
```

The transforms given here are intended to show how they can be composed: they
are applied in the order provided, and the document is transformed using a
depth-first, bottom-up traversal. Put another way, you transform the leaves of
the tree before the branches; as shown in the example above, this means the
`:ip` key is converted to an address tuple before the `:servers` key is
transformed into a list of `Server` structs.

## Using with Elixir Releases (1.9+)

To use this library as a configuration provider in Elixir, use the following
example of how one might use it in their release configuration, and tailor it
to your needs:

```elixir
config_providers: [
  {Toml.Provider, [
    path: {:system, "XDG_CONFIG_DIR", "myapp.toml"},
    transforms: [...]
  ]}
]
```

See the "Using as a Config Provider" section for more info.

## Using with Distillery

Like the above, use the following example as a guideline for how you use this
in your own release configuration (i.e. in `rel/config.exs`):

``` elixir
release :myapp do
  # ...snip...
  set config_providers: [
    {Toml.Provider, [path: "${XDG_CONFIG_DIR}/myapp.toml", transforms: [...]]}
  ]
end
```

## Using as a Config Provider

The usages described above will result in `Toml.Provider` being invoked during boot, at which point it
will evaluate the given path and read the TOML file it finds. If one is not
found, or is not accessible, the provider will raise an error, and the boot
sequence will terminate unsuccessfully. If it succeeds, it persists settings in
the file to the application environment (i.e. you access it via
`Application.get_env/2`).

You can pass the same options in the arguments list for `Toml.Provider` as you
can to `Toml.decode/2`, but `:path` is required, and `:keys` only supports
`:atoms` and `:atoms!` values.

The config provider expects a certain format to the TOML file, namely that keys
at the root of the document correspond to applications which need to be configured.
If it encounters keys at the root of the document which are not tables, they are ignored.

``` toml
# This is an example of something that would be ignored
title = "My config file"

# We're expecting something like this:
[myapp]
key = "value"

# To use a bit of Phoenix config, you translate to TOML like so:
[myapp."MyApp.Endpoint"]
cache_static_manifest = "priv/static/cache_manifest.json"

[myapp."MyApp.Endpoint".http]
port = "4000"

[myapp."MyApp.Endpoint".force_ssl]
hsts = true

# Or logger..
[logger]
level = "info"

[logger.console]
format = "[$level] $message \n"
```

## Roadmap

- [x] Add benchmarking suite
- [x] Provide options for converting keys to atom, similar to Jason/Poison/etc.
- [ ] Optimize lexer to always send offsets to decoder, rather than only in some cases
- [ ] Try to find pathological TOML files to test

## License

This project is licensed Apache 2.0, see the `LICENSE` file in this repo for details.
