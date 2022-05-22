defmodule AdventOfCode.Day02Test do
  use ExUnit.Case

  setup do
    [input: "test/day02/input.txt"]
  end

  @tag :skip
  test "part1", %{input: file} do
    ["2x3x4"]
    |> AdventOfCode.Day02.part1()
    |> (&assert(&1 === 58)).()

    ["1x1x10"]
    |> AdventOfCode.Day02.part1()
    |> (&assert(&1 === 43)).()

    IO.puts("Actual result part 1:")

    File.read!(file)
    |> String.split("\n", trim: true)
    |> AdventOfCode.Day02.part1()
    |> IO.puts()
  end

  # @tag :skip
  test "part2", %{input: file} do
    ["2x3x4"]
    |> AdventOfCode.Day02.part2()
    |> (&assert(&1 === 34)).()

    ["1x1x10"]
    |> AdventOfCode.Day02.part2()
    |> (&assert(&1 === 14)).()

    IO.puts("Actual result part 2:")

    File.read!(file)
    |> String.split("\n", trim: true)
    |> AdventOfCode.Day02.part2()
    |> IO.puts()
  end
end
