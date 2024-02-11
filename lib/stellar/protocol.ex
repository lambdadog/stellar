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

      {:ok, decode_payload(payload), mac}
    rescue
      e in RuntimeError -> {:error, e}
    end
  end

  # KEXINIT
  # byte         SSH_MSG_KEXINIT
  # byte[16]     cookie (random bytes)
  # name-list    kex_algorithms
  # name-list    server_host_key_algorithms
  # name-list    encryption_algorithms_client_to_server
  # name-list    encryption_algorithms_server_to_client
  # name-list    mac_algorithms_client_to_server
  # name-list    mac_algorithms_server_to_client
  # name-list    compression_algorithms_client_to_server
  # name-list    compression_algorithms_server_to_client
  # name-list    languages_client_to_server
  # name-list    languages_server_to_client
  # boolean      first_kex_packet_follows
  # uint32       0 (reserved for future extension)
  def decode_payload(<<20, cookie::binary-size(16), rest::binary>>) do
    {kex_algs, rest} = decode_namelist(rest)
    {server_hk_algs, rest} = decode_namelist(rest)
    {encryption_algs_c2s, rest} = decode_namelist(rest)
    {encryption_algs_s2c, rest} = decode_namelist(rest)
    {mac_algs_c2s, rest} = decode_namelist(rest)
    {mac_algs_s2c, rest} = decode_namelist(rest)
    {compression_algs_c2s, rest} = decode_namelist(rest)
    {compression_algs_s2c, rest} = decode_namelist(rest)
    {languages_c2s, rest} = decode_namelist(rest)
    {languages_s2c, rest} = decode_namelist(rest)
    {first_kex_packet_follows, rest} = decode_binary(rest)

    <<_::integer-size(32)>> = rest

    {
      :kexinit,
      %{
	cookie: cookie,
	kex_algs: kex_algs,
	server_hk_algs: server_hk_algs,
	encryption_algs_c2s: encryption_algs_c2s,
	encryption_algs_s2c: encryption_algs_s2c,
	mac_algs_c2s: mac_algs_c2s,
	mac_algs_s2c: mac_algs_s2c,
	compression_algs_c2s: compression_algs_c2s,
	compression_algs_s2c: compression_algs_s2c,
	languages_c2s: languages_c2s,
	languages_s2c: languages_s2c,
	first_kex_packet_follows: first_kex_packet_follows
      }
    }
  end

  def decode_namelist(<<0::integer-size(32), rest::binary>>),
    do: {[], rest}

  def decode_namelist(<<l::integer-size(32), str::binary-size(l), rest::binary>>),
    do: {String.split(str, ","), rest}

  def decode_binary(<<1, rest::binary>>),
    do: {true, rest}

  def decode_binary(<<0, rest::binary>>),
    do: {false, rest}
end
