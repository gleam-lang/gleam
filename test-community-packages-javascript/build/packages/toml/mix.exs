defmodule Toml.MixProject do
  use Mix.Project

  @version "0.7.0"
  @source_url "https://github.com/bitwalker/toml-elixir"

  def project do
    [
      app: :toml,
      version: @version,
      elixir: "~> 1.9",
      start_permanent: Mix.env() == :prod,
      consolidate_protocols: Mix.env() != :test,
      description: "An implementation of TOML for Elixir projects",
      package: package(),
      deps: deps(),
      aliases: aliases(Mix.env()),
      preferred_cli_env: [
        bench: :bench,
        "bench.decoder": :bench,
        "bench.lexer": :bench,
        docs: :docs,
        "hex.publish": :docs,
        coveralls: :test,
        "coveralls.html": :test,
        "coveralls.details": :test
      ],
      dialyzer: dialyzer(),
      docs: docs(),
      elixirc_paths: elixirc_paths(Mix.env()),
      escript: escript(Mix.env()),
      test_coverage: [tool: ExCoveralls]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: []
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:ex_doc, ">= 0.0.0", only: [:docs]},
      {:dialyxir, "~> 1.0", only: [:dev, :test], runtime: false},
      {:benchee, "~> 1.0", only: [:bench]},
      {:benchee_html, "~> 1.0", only: [:bench]},
      {:jason, "~> 1.0", only: [:test, :bench]},
      {:excoveralls, "~> 0.9", only: [:test]}
      # For benchmarking, though none of these libraries work at this point
      # {:tomlex, "~> 0.0.5", only: [:bench]},
      # {:toml_elixir, "~> 2.0.1", only: [:bench]},
      # {:jerry, "~> 0.1.4", only: [:bench]},
      # {:etoml, "~> 0.1.0", only: [:bench]},
    ]
  end

  defp package do
    [
      files: ["lib", "mix.exs", "README.md", "LICENSE"],
      maintainers: ["Paul Schoenfelder"],
      licenses: ["Apache-2.0"],
      links: %{"GitHub" => @source_url}
    ]
  end

  defp docs do
    [
      main: "readme",
      source_url: @source_url,
      source_ref: @version,
      extras: [
        LICENSE: [title: "License"],
        "README.md": [title: "Overview"]
      ],
      formatters: ["html"]
    ]
  end

  defp escript(:test) do
    [
      main_module: Toml.CLI,
      name: :toml,
      path: Path.join([__DIR__, "bin", "toml"])
    ]
  end

  defp escript(_), do: nil

  defp aliases(_env) do
    [
      "compile-check": [
        "compile --warnings-as-errors",
        "format --check-formatted --dry-run",
        "dialyzer --format dialyxir"
      ],
      clean: ["clean", &clean/1],
      bench: ["bench.decoder", "bench.lexer"],
      "bench.decoder": ["run bench/bench.decoder.exs"],
      "bench.lexer": ["run bench/bench.lexer.exs"]
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(:bench), do: ["lib", "bench/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp dialyzer do
    [
      ignore_warnings: "dialyzer.ignore",
      flags: [:error_handling, :underspecs],
      plt_core_path: System.get_env("PLT_PATH") || System.get_env("MIX_HOME")
    ]
  end

  defp clean(_args) do
    toml = Path.join([__DIR__, "bin", "toml"])

    if File.exists?(toml) do
      _ = File.rm(toml)
    end
  end
end
