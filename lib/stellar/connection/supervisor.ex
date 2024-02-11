defmodule Stellar.Connection.Supervisor do
  @moduledoc false

  use Supervisor
  @behaviour :ranch_protocol

  require Logger

  @type options :: []

  @impl true
  @spec start_link(:ranch.ref(), module(), options()) :: Supervisor.on_start()
  def start_link(ref, transport, %{timeout: timeout} = opts) do
    conn_opts = %{
      timeout: timeout
    }

    {:ok, sup} = Supervisor.start_link(__MODULE__, opts)

    {:ok, conn} =
      Supervisor.start_child(
        sup,
        {Stellar.Connection, [ref, transport, conn_opts]}
      )

    {:ok, sup, conn}
  end

  @impl true
  def init(_opts) do
    children = []

    # TODO: I want this supervisor to kill all children and die if the
    # Stellar.Connection child dies, but I'm not sure how (or if ranch
    # will do it automatically)
    Supervisor.init(children, strategy: :one_for_one)
  end
end
