defmodule AdventOfCode.Day03 do
  @spec moving(list(String.t())) :: MapSet.t({integer, integer})
  defp moving(input) do
    update_map = fn {houses, {x, y}}, {vx, vy} ->
      nx = x + vx
      ny = y + vy

      houses =
        case MapSet.member?(houses, {nx, ny}) do
          false -> MapSet.put(houses, {nx, ny})
          _ -> houses
        end

      {houses, {nx, ny}}
    end

    input
    |> Enum.reduce({_houses = MapSet.new([{0, 0}]), {_x = 0, _y = 0}}, fn
      ">", acc -> update_map.(acc, {1, 0})
      "v", acc -> update_map.(acc, {0, -1})
      "<", acc -> update_map.(acc, {-1, 0})
      "^", acc -> update_map.(acc, {0, 1})
    end)
    |> then(fn {houses, _} -> houses end)
  end

  @spec part1(list(String.t())) :: integer
  def part1(input) do
    input
    |> moving()
    |> MapSet.size()
  end

  @spec part2(list(String.t())) :: integer
  def part2(input) do
    for_santa = fn {_, i} -> rem(i, 2) == 0 end

    [santa, robo_santa] =
      input
      |> Enum.with_index()
      |> Enum.split_with(for_santa)
      |> Tuple.to_list()
      |> Enum.map(fn ops ->
        ops
        |> Enum.map(fn {op, _} -> op end)
        |> moving()
      end)

    MapSet.union(santa, robo_santa) |> MapSet.size()
  end
end
