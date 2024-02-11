defmodule Stellar.Protocol do
  require Logger
  # TODO: encryption
  def decode_packet_length(data) do
    if byte_size(data) < 4 do
      :continue
    else
      <<length::integer-size(32), rest::binary>> = data

      {:ok, length, rest}
    end
  end

  def decode_packet(packet_length, mac_length, data) do
    try do
      <<pad_length::integer, data::binary>> = data

      # The length of the packet, minus padding, minus the size of
      # pad_length (one byte).
      payload_length = packet_length - pad_length - 1
      <<payload::binary-size(payload_length), data::binary>> = data

      <<_pad::binary-size(pad_length), mac::binary-size(mac_length)>> = data

      {:ok, payload, mac}
    rescue
      e in RuntimeError -> {:error, e}
    end
  end
end
