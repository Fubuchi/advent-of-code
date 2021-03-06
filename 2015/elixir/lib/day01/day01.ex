defmodule AdventOfCode.Day01 do
  @spec part1(list(String.t())) :: integer
  def part1(input) do
    input
    |> Enum.reduce(_floor = 0, fn
      "(", floor -> floor + 1
      ")", floor -> floor - 1
    end)
  end

  @spec part2(list(String.t())) :: integer
  def part2(input) do
    input
    |> Enum.with_index(_offset = 1)
    |> Enum.reduce_while({_floor = 0, _pos = -1}, fn
      {_, _}, {-1, pos} -> {:halt, {-1, pos}}
      {"(", i}, {floor, _} -> {:cont, {floor + 1, i}}
      {")", i}, {floor, _} -> {:cont, {floor - 1, i}}
    end)
    |> then(fn
      {-1, pos} -> pos
      _ -> -1
    end)
  end
end
