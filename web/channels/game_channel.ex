defmodule Hello.GameChannel do
    use Hello.Web, :channel

    intercept ["play.card"]

    def join("game:" <> game_id, _params, socket) do
        IO.puts "Join Game " <> game_id
        send(self, :after_join)
        {:ok, assign(socket, :game_id, String.to_integer(game_id))}
    end

    def handle_info(:ping, socket) do
        IO.puts "handle info"
        count = socket.assigns[:count] || 1
        push socket, "ping", %{count: count}
        {:noreply, assign(socket, :count, count + 1)}
    end

    def handle_info(:after_join, socket) do
        IO.puts "handle info"
        votes = socket.assigns[:votes] || %{}
        IO.inspect votes
        push socket, "list.card", votes
        {:noreply, assign(socket, :votes, votes)}
    end

    def handle_in("play.card", %{"number" => number, "user" => user}, socket) do
        IO.puts "#{user} played #{number}"
        votes = socket.assigns[:votes] || %{}
        votes = Map.put(votes, user, number)
        IO.puts "add vote"
        IO.inspect votes
        broadcast! socket, "play.card", %{number: number, user: user}
        {:noreply, assign(socket, :votes, votes)}
    end

    def handle_out("play.card", payload, socket) do
        IO.puts "Push card play"
        push socket, "play.card", payload
        {:noreply, socket}
    end

end
