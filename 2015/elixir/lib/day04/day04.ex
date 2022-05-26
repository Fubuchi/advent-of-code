defmodule AdventOfCode.Day04 do
  @step 100_000

  defp hash(text, search, counter, limit) do
    if counter > limit do
      :not_found
    else
      case :crypto.hash(:md5, text <> to_string(counter))
           |> Base.encode16()
           |> String.starts_with?(search) do
        true -> {:found, counter}
        _ -> hash(text, search, counter + 1, limit)
      end
    end
  end

  defp spawn_hash(pid, text, search, start, limit) do
    {spawn(fn ->
       send(pid, {self(), hash(text, search, start, limit)})
     end), limit}
  end

  defp loop(workers, text, search) do
    receive do
      {_, {:found, result}} ->
        {keeps, kills} =
          workers
          |> Enum.reduce({[], []}, fn {pid, limit}, {keeps, kills} ->
            if limit > result do
              {keeps, [pid | kills]}
            else
              {[{pid, limit} | keeps], kills}
            end
          end)

        kills |> Enum.each(&Process.exit(&1, :kill))

        if workers |> Enum.all?(fn {pid, _} -> !Process.alive?(pid) end) do
          result
        else
          loop(keeps, text, search)
        end

      {sender, :not_found} ->
        Process.exit(sender, :kill)

        keeps =
          workers
          |> Enum.filter(fn {pid, _} -> Process.alive?(pid) end)

        max_limit =
          workers
          |> Enum.max_by(fn {_, limit} -> limit end)
          |> elem(1)

        next_limit = max_limit + @step

        loop(
          [spawn_hash(self(), text, search, max_limit, next_limit) | keeps],
          text,
          search
        )
    end
  end

  defp run(input, search) do
    schedulers = System.schedulers_online()

    for i <- 0..schedulers do
      i * @step
    end
    |> Enum.chunk_every(2, 1, :discard)
    |> Enum.map(fn [start, limit] -> spawn_hash(self(), input, search, start, limit) end)
    |> loop(input, search)
  end

  @spec part1(String.t()) :: integer
  def part1(input) do
    run(input, "00000")
  end

  @spec part2(String.t()) :: integer
  def part2(input) do
    run(input, "000000")
  end
end
