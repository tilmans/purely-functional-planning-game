defmodule Hello.GameChannel do
    use Hello.Web, :channel

    intercept ["play.card"]

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

    def handle_in("play.card", %{"number" => number, "user" => user}, socket) do
        IO.puts "#{user} played #{number}"
        broadcast! socket, "play.card", %{number: number, user: user}
        {:noreply, socket}
    end

    def handle_out("play.card", payload, socket) do
        IO.puts "Push card play"
        push socket, "play.card", payload
        {:noreply, socket}
    end

end
