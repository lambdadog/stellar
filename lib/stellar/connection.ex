defmodule Stellar.Connection do
  @behaviour :gen_statem

  require Logger

  @crlf "\u000d\u000a"
  @version_string "SSH-2.0-Stellar_0.1.0" <> @crlf

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

    state_data = %{
      prev: nil
    }

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
    do: [:state_functions, :state_enter]

  def version_exchange(:enter, :version_exchange, state_data),
    do: {:keep_state, state_data}

  def version_exchange(:info, {:tcp_closed, _socket}, _state_data),
    do: {:stop, :normal}

  def version_exchange(:info, {:tcp_error, _, reason}, _state_data),
    do: {:stop, reason}

  def version_exchange(
    :info,
    {:tcp, socket, message},
    {%{prev: prev} = state_data, socket, transport}
  ) do
    # If we received a partial message previously, merge it with our
    # current message.
    message = if is_nil(prev) do message else prev <> message end

    {message, rest} = if String.ends_with?(message, @crlf) do
      {message, nil}
    else
      case String.split(message, @crlf, parts: 2) do
	[message, rest] -> {message, rest}
	[rest] -> {nil, rest}
      end
    end

    state_data = %{state_data | prev: rest}

    case message do
      <<"SSH-", version_string::binary>> ->
	case String.split(version_string, ["-", @crlf, " "], parts: 3) do
	  [ssh_version, client_version, _] ->
	    Logger.debug("SSH version: #{ssh_version}")
	    Logger.debug("Client version: #{client_version}")

	    if ssh_version in ["2.0", "1.99"] do
	      # We wait until we receive a version message to reply,
	      # to avoid giving away what type of service is running
	      # on this socket.

	      # In theory this could break compatibility with a client
	      # that waits on the server message to respond, but that
	      # can be revisited if it becomes a problem.
	      :ok = transport.send(socket, @version_string)
	      :ok = transport.setopts(socket, active: :once)
	      {
		:next_state,
		:key_exchange,
		{state_data, socket, transport}
	      }
	    else
	      {
		:stop,
		{:incompatible_client_version, ssh_version},
		{state_data, socket, transport}
	      }
	    end
	  _ ->
	    {
	      :stop,
	      :version_parse_error,
	      {state_data, socket, transport}
	    }
	end

      nil ->
	{:keep_state, {state_data, socket, transport}}
    end
  end

  # TODO: improve
  def version_exchange(event, msg, state_data) do
    Logger.debug("Unhandled message during version_exchange")
    Logger.debug("#{inspect event}\n#{inspect msg}\n#{inspect state_data}")

    :keep_state_and_data
  end

  def key_exchange(
    :enter,
    :version_exchange,
    {_vex_data, socket, transport}
  ) do
    # Intentionally discards old prev. If this causes issues then we
    # can revisit it but Version Exchange and Key Exchange SHOULD be
    # separate steps. Any client sending both at once is being a bit
    # too clever.

    # TODO: go ahead and send our own kexinit, we don't need to wait
    # on the client
    state_data = %{
      packet_length: nil,
      prev: nil
    }

    {:keep_state, {state_data, socket, transport}}
  end

  def key_exchange(:info, {:tcp_closed, _socket}, _state_data),
    do: {:stop, :normal}

  def key_exchange(:info, {:tcp_error, _, reason}, _state_data),
    do: {:stop, reason}

  def key_exchange(
    :info,
    {:tcp, socket, message},
    {%{packet_length: p_length, prev: prev} = state_data, socket, transport}
  ) do
    message = if is_nil(prev) do message else prev <> message end

    # TODO: breakout
    {p_length, message} = if is_nil(p_length) do
      case Stellar.Protocol.decode_packet_length(message) do
	{:ok, p_length, rest} ->
	  {p_length, rest}
	:continue ->
	  {nil, message}
      end
    else
      {p_length, message}
    end

    # There's no Message Authentication algorithm at this point (we
    # are still negotiating that) so there's no need to add the length
    # of the MAC to the packet length, although if we generalize this
    # code we will want to
    if is_nil(p_length) or byte_size(message) < p_length do
      # Wait for rest of packet
      :ok = transport.setopts(socket, active: :once)
      {
	:keep_state,
	{
	  %{state_data | packet_length: p_length, prev: message},
	  socket,
	  transport
	}
      }
    else
      {:ok, payload, _mac} = Stellar.Protocol.decode_packet(p_length, 0, message)

      Logger.debug("payload:\n#{inspect payload, pretty: true}")

      # TODO: continue
      {:stop, :normal}
    end
  end

  def key_exchange(event, msg, state_data) do
    Logger.debug("Unhandled message during key_exchange")
    Logger.debug("#{inspect event}\n#{inspect msg}\n#{inspect state_data}")

    :keep_state_and_data
  end
end
