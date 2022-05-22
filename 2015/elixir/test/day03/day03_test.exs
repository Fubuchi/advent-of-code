defmodule AdventOfCode.Day03Test do
  use ExUnit.Case

  setup do
    [input: "test/day03/input.txt"]
  end

  @tag :skip
  test "part1", %{input: file} do
    "^>v<"
    |> String.graphemes()
    |> AdventOfCode.Day03.part1()
    |> (&assert(&1 === 4)).()

    "^v^v^v^v^v"
    |> String.graphemes()
    |> AdventOfCode.Day03.part1()
    |> (&assert(&1 === 2)).()

    IO.puts("Actual result part 1:")

    File.read!(file)
    |> String.split("\n")
    |> Enum.at(0)
    |> String.graphemes()
    |> AdventOfCode.Day03.part1()
    |> IO.puts()
  end

  @tag :skip
  test "part2", %{input: file} do
    "^>v<"
    |> String.graphemes()
    |> AdventOfCode.Day03.part2()
    |> (&assert(&1 === 3)).()

    "^v^v^v^v^v"
    |> String.graphemes()
    |> AdventOfCode.Day03.part2()
    |> (&assert(&1 === 11)).()

    IO.puts("Actual result part 2:")

    File.read!(file)
    |> String.split("\n")
    |> Enum.at(0)
    |> String.graphemes()
    |> AdventOfCode.Day03.part2()
    |> IO.puts()
  end
end
