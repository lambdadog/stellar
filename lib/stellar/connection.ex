defmodule Stellar.Connection do
  @moduledoc false

  use GenServer

  require Logger
  alias Stellar.SSH, as: SSH

  # Technically untrue since we know we're using the :ranch_tcp
  # transport, but since ranch isn't designed that way...
  @type socket :: any()

  @type state :: [
          transport: module(),
          socket: socket(),
          protocol_state: any(),
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
      protocol_state: SSH.Protocol.init_state(),
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
          protocol_state: p_state,
          timeout: timeout
        } = state
      ) do
    case SSH.Protocol.read(p_state, data) do
      {:ok, p_state, result} ->
        case result do
          {:version, {ssh_version, _client_version} = version} ->
            Logger.debug("Client version: #{inspect(version)}")

	    # Ensure compatible SSH version
            if ssh_version in ["2.0", "1.99"] do
              :ok = transport.send(socket, SSH.Protocol.version_string())
              :ok = transport.setopts(socket, active: :once)
              {:noreply, %{state | protocol_state: p_state}, timeout}
            else
              {
                :stop,
                {:incompatible_client_version, ssh_version},
                %{state | protocol_state: p_state}
              }
            end

          _ ->
	    {
	      :stop,
	      {:unexpected_parse_return, result},
	      %{state | protocol_state: p_state}
	    }
        end

      {:continue, p_state} ->
        :ok = transport.setopts(socket, active: :once)
        {:noreply, %{state | protocol_state: p_state}, timeout}

      {:error, reason} ->
        {:stop, reason, state}
    end
  end

  @impl true
  def handle_info(message, state) do
    Logger.debug("handle_info: #{inspect(message)}")
    {:stop, :shutdown, state}
  end

  @impl true
  def terminate(reason, %{transport: transport, socket: socket}) do
    Logger.debug("terminating for reason: #{inspect(reason)}")
    transport.close(socket)
  end
end
