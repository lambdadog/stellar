defmodule Stellar do
  @moduledoc """
  Documentation for `Stellar`.
  """

  @type options :: [
          port: :inet.port_number(),
          timeout: timeout()
        ]

  @spec child_spec(options()) :: Supervisor.child_spec()
  def child_spec(%{port: port, timeout: timeout}) do
    ranch_opts = %{
      socket_opts: [
        {:port, port}
      ],
      connection_type: :supervisor
    }

    module_opts = %{
      timeout: timeout
    }

    :ranch.child_spec(
      __MODULE__,
      :ranch_tcp,
      ranch_opts,
      Stellar.Connection.Supervisor,
      module_opts
    )
  end

  def run_devel do
    children = [
      child_spec(%{port: 8888, timeout: :infinity})
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end
end
