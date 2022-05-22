defmodule AdventOfCode.Day01Test do
  use ExUnit.Case

  setup do
    [input: "test/day01/input.txt"]
  end

  @tag :skip
  test "part1", %{input: file} do
    "(())"
    |> String.graphemes()
    |> AdventOfCode.Day01.part1()
    |> (&assert(&1 === 0)).()

    ")())())"
    |> String.graphemes()
    |> AdventOfCode.Day01.part1()
    |> (&assert(&1 === -3)).()

    IO.puts("Actual result part 1:")

    File.read!(file)
    |> String.split("\n")
    |> Enum.at(0)
    |> String.graphemes()
    |> AdventOfCode.Day01.part1()
    |> IO.puts()
  end

  @tag :skip
  test "part2", %{input: file} do
    ")"
    |> String.graphemes()
    |> AdventOfCode.Day01.part2()
    |> (&assert(&1 === 1)).()

    "()())"
    |> String.graphemes()
    |> AdventOfCode.Day01.part2()
    |> (&assert(&1 === 5)).()

    IO.puts("Actual result part 2:")

    File.read!(file)
    |> String.split("\n")
    |> Enum.at(0)
    |> String.graphemes()
    |> AdventOfCode.Day01.part2()
    |> IO.puts()
  end
end
