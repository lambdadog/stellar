defmodule Stellar.Connection do
  @moduledoc false

  use GenServer

  import Logger

  # Technically untrue since we know we're using the :ranch_tcp
  # transport, but since ranch isn't designed that way...
  @type socket :: any()

  @type state :: [
          transport: module(),
          socket: socket(),
          timeout: timeout()
        ]

  def start_link(init_arg),
    do: GenServer.start_link(__MODULE__, init_arg)

  @impl true
  def init({ref, transport, opts}),
    # Socket is not ready until after continue.
    do: {:ok, {}, {:continue, {ref, transport, opts}}}

  @impl true
  def handle_continue(
        {ref, transport, %{timeout: timeout}},
        _state
      ) do
    {:ok, socket} = :ranch.handshake(ref)

    state = %{
      transport: transport,
      socket: socket,
      timeout: timeout
    }

    :ok = transport.setopts(socket, active: :once)
    {:noreply, state, timeout}
  end

  @impl true
  def handle_info(
        {:tcp, socket, data},
        %{
          transport: transport,
          socket: socket,
          timeout: timeout
        } = state
      ) do
    :ok = transport.send(socket, data)

    :ok = transport.setopts(socket, active: :once)
    {:noreply, state, timeout}
  end

  @impl true
  def handle_info(
        message,
        %{
          transport: transport,
          socket: socket
        } = state
      ) do
    Logger.debug("handle_info: #{inspect(message)}")
    transport.close(socket)
    {:stop, :shutdown, state}
  end
end
