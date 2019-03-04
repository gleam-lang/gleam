{_, 0} = System.cmd("gleam", ["build", "."], into: IO.binstream(:stdio, :line))

defmodule WebApp.MixProject do
  use Mix.Project

  def project do
    [
      app: :web_app,
      version: "0.1.0",
      elixir: "~> 1.8",
      start_permanent: Mix.env() == :prod,
      erlc_paths: ["src", "gen"],
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {WebApp.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:plug, "~> 1.6"},
      {:plug_cowboy, "~> 2.0"}
    ]
  end
end
