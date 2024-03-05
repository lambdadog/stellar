defmodule Stellar.Connection.Supervisor do
  @moduledoc false

  use Supervisor
  @behaviour :ranch_protocol

  require Logger

  @type options :: []

  @impl true
  @spec start_link(:ranch.ref(), module(), options()) :: Supervisor.on_start()
  def start_link(ref, transport, %{timeout: timeout} = _opts) do
    conn_opts = %{
      timeout: timeout
    }

    {:ok, sup} = Supervisor.start_link(
      __MODULE__,
      strategy: :one_for_one,
      auto_shutdown: :any_significant
    )

    {:ok, conn} =
      Supervisor.start_child(
        sup,
        {Stellar.Connection, [ref, transport, conn_opts]}
      )

    {:ok, sup, conn}
  end

  @impl true
  def init(opts) do
    children = []

    Supervisor.init(children, opts)
  end
end
