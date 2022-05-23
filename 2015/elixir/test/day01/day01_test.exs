defmodule AdventOfCode.Day01Test do
  use ExUnit.Case

  setup do
    [input: "test/day01/input.txt"]
  end

  test "part1", %{input: file} do
    "(())"
    |> String.graphemes()
    |> AdventOfCode.Day01.part1()
    |> (&assert(&1 === 0)).()

    ")())())"
    |> String.graphemes()
    |> AdventOfCode.Day01.part1()
    |> (&assert(&1 === -3)).()

    File.read!(file)
    |> String.split("\n")
    |> Enum.at(0)
    |> String.graphemes()
    |> AdventOfCode.Day01.part1()
    |> (&assert(&1 == 232)).()
  end

  test "part2", %{input: file} do
    ")"
    |> String.graphemes()
    |> AdventOfCode.Day01.part2()
    |> (&assert(&1 === 1)).()

    "()())"
    |> String.graphemes()
    |> AdventOfCode.Day01.part2()
    |> (&assert(&1 === 5)).()

    File.read!(file)
    |> String.split("\n")
    |> Enum.at(0)
    |> String.graphemes()
    |> AdventOfCode.Day01.part2()
    |> (&assert(&1 == 1783)).()
  end
end
