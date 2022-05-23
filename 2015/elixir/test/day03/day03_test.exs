defmodule AdventOfCode.Day03Test do
  use ExUnit.Case

  setup do
    [input: "test/day03/input.txt"]
  end

  test "part1", %{input: file} do
    "^>v<"
    |> String.graphemes()
    |> AdventOfCode.Day03.part1()
    |> (&assert(&1 === 4)).()

    "^v^v^v^v^v"
    |> String.graphemes()
    |> AdventOfCode.Day03.part1()
    |> (&assert(&1 === 2)).()

    File.read!(file)
    |> String.split("\n")
    |> Enum.at(0)
    |> String.graphemes()
    |> AdventOfCode.Day03.part1()
    |> (&assert(&1 === 2081)).()
  end

  test "part2", %{input: file} do
    "^>v<"
    |> String.graphemes()
    |> AdventOfCode.Day03.part2()
    |> (&assert(&1 === 3)).()

    "^v^v^v^v^v"
    |> String.graphemes()
    |> AdventOfCode.Day03.part2()
    |> (&assert(&1 === 11)).()

    File.read!(file)
    |> String.split("\n")
    |> Enum.at(0)
    |> String.graphemes()
    |> AdventOfCode.Day03.part2()
    |> (&assert(&1 === 2341)).()
  end
end
