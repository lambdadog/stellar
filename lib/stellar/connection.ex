defmodule Stellar.Connection do
  @behaviour :gen_statem

  require Logger

  def child_spec(init_args),
    do: %{
	  id: __MODULE__,
	  start: {__MODULE__, :start_link, init_args},
	  restart: :temporary,
	  significant: true
    }

  def start_link(ref, transport, opts),
    do: {:ok, :proc_lib.spawn_link(__MODULE__, :init, [{ref, transport, opts}])}

  @impl :gen_statem
  def init({ref, transport, _opts}) do
    {:ok, socket} = :ranch.handshake(ref)

    {:ok, state_data} = SSHProto.init(%SSHProto.Config{})

    :ok = transport.setopts(socket, active: :once)
    :gen_statem.enter_loop(
      __MODULE__,
      [],
      :version_exchange,
      {state_data, socket, transport}
    )
  end

  @impl :gen_statem
  def callback_mode,
    do: [:handle_event_function, :state_enter]

  @impl :gen_statem
  def handle_event(:info, {:tcp_closed, _socket}, _state, _state_data),
    do: {:stop, :normal}

  def handle_event(:info, {:tcp_error, _, reason}, _state, _state_data),
    do: {:stop, reason}

  def handle_event(:enter, :version_exchange, :version_exchange, state_data) do
    {_, socket, transport} = state_data

    {:ok, version_message} = SSHProto.encode_version("Stellar_0.1.0")
    :ok = transport.send(socket, version_message)

    {:keep_state, state_data}
  end

  def handle_event(
    :info,
    {:tcp, socket, message},
    :version_exchange,
    {ssh_state, socket, transport}
  ) do
    # Assume the version is sent in one message. I'm not entirely sure
    # if it's possible for it to be multiple messages.
    {:ok, client_version} = SSHProto.decode_version(message)

    Logger.debug("Client Version: #{client_version}")

    :ok = transport.setopts(socket, active: :once)

    {
      :next_state,
      :key_exchange,
      {ssh_state, socket, transport}
    }
  end

  def handle_event(:enter, :version_exchange, :key_exchange, state_data) do
    # TODO: Send KEXINIT on our end

    {:keep_state, state_data}
  end

  def handle_event(
    :info,
    {:tcp, socket, message},
    :key_exchange,
    {ssh_state, socket, transport}
  ) do
    case SSHProto.decode(ssh_state, message) do
      {:continue, ssh_state} ->
	:ok = transport.setopts(socket, active: :once)

	{:keep_state, {ssh_state, socket, transport}}

      {:error, e} ->
	{:stop, e}

      {:ok, message, ssh_state} ->
	Logger.debug(inspect message)

	:ok = transport.setopts(socket, active: :once)

	{:keep_state, ssh_state}
    end
  end
end
