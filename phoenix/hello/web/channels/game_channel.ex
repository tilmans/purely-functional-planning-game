defmodule Hello.GameChannel do
    use Hello.Web, :channel

    def join("game:1", _params, socket) do
        IO.puts "Join Game Base"
        {:ok, socket}
    end

    def join("game:" <> game_id, _params, socket) do
        IO.puts "Join Game " <> game_id
        {:ok, assign(socket, :game_id, String.to_integer(game_id))}
    end

    def handle_info(:ping, socket) do
        IO.puts "handle info"
        count = socket.assigns[:count] || 1
        push socket, "ping", %{count: count}

        {:noreply, assign(socket, :count, count + 1)}
    end

    def handle_in("new.msg", _params, socket) do
        IO.puts "Someone said hello"
        {:reply, :ok, socket}
    end

end
