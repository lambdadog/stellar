defmodule Stellar.SSH.Protocol do
  @moduledoc false

  require Logger

  @crlf "\u000d\u000a"

  @type state :: [
          stage: any(),
          prev: binary() | nil
        ]

  # API

  def init_state(),
    do: %{stage: :init, prev: nil}

  # TODO: Substitute version
  def version_string(),
    do: "SSH-2.0-Stellar_0.1.0#{@crlf}"

  @spec read(state(), binary()) :: {:ok, state(), any()} | {:continue, state()} | {:error, any()}
  def read(%{prev: prev} = state, data) when is_binary(prev),
    do: read(%{state | prev: nil}, prev <> data)

  def read(%{stage: :init} = state, data) do
    {msg, rest} =
      if String.ends_with?(data, @crlf) do
        {data, nil}
      else
        case String.split(data, @crlf, parts: 2) do
          [msg, rest] -> {msg, rest}
          [rest] -> {nil, rest}
        end
      end

    case msg do
      <<"SSH-", _::binary>> ->
        case parse_version(msg) do
          {:ok, version} ->
            {:ok, %{state | stage: :version_exchanged, prev: rest}, {:version, version}}

          :error ->
            {:error, :version_parse_failure}
        end

      nil ->
        {:continue, %{state | prev: rest}}

      _ ->
        {:error, :illegal_message_before_version_exchange}
    end
  end

  def read(%{stage: stage}, _data) do
    {:error, {:unimplemented_stage, stage}}
  end

  def parse_version(data) do
    case String.split(data, ["-", @crlf, " "], parts: 4) do
      [_, ssh_version, client_version, _] ->
        {:ok, {ssh_version, client_version}}

      _ ->
        :error
    end
  end
end
