defmodule Stellar.MixProject do
  use Mix.Project

  def project do
    [
      app: :stellar,
      version: "0.1.0",
      elixir: "~> 1.16",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger, :crypto]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:ranch, "~> 2.1"},
      {:ssh_proto, path: "../ssh_proto"}
    ]
  end
end
