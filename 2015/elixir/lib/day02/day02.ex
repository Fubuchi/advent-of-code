defmodule AdventOfCode.Day02 do
  defmodule Utils do
    @spec comb(list, integer) :: list
    def comb(_, 0), do: [[]]
    def comb([], _), do: []

    def comb([h | t], m) do
      for(l <- comb(t, m - 1), do: [h | l]) ++ comb(t, m)
    end
  end

  @spec part1(list) :: integer
  def part1(input) do
    input
    |> Enum.map(fn s ->
      String.split(s, "x")
      |> Enum.map(&String.to_integer/1)
      |> Utils.comb(2)
      |> Enum.map(fn [x, y] -> x * y end)
    end)
    |> Enum.map(fn
      l -> 2 * (l |> Enum.sum()) + (l |> Enum.min())
    end)
    |> Enum.sum()
  end

  @spec part2(list) :: integer
  def part2(input) do
    input
    |> Enum.map(fn s ->
      String.split(s, "x")
      |> Enum.map(&String.to_integer/1)
    end)
    |> Enum.map(fn l ->
      [x, y] = l |> Enum.sort() |> Enum.take(2)
      x + x + y + y + (l |> Enum.reduce(1, fn x, a -> x * a end))
    end)
    |> Enum.sum()
  end
end
