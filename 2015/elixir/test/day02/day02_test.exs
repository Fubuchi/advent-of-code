defmodule AdventOfCode.Day02Test do
  use ExUnit.Case

  setup do
    [input: "test/day02/input.txt"]
  end

  test "part1", %{input: file} do
    ["2x3x4"]
    |> AdventOfCode.Day02.part1()
    |> (&assert(&1 === 58)).()

    ["1x1x10"]
    |> AdventOfCode.Day02.part1()
    |> (&assert(&1 === 43)).()

    File.read!(file)
    |> String.split("\n", trim: true)
    |> AdventOfCode.Day02.part1()
    |> (&assert(&1 === 1_598_415)).()
  end

  test "part2", %{input: file} do
    ["2x3x4"]
    |> AdventOfCode.Day02.part2()
    |> (&assert(&1 === 34)).()

    ["1x1x10"]
    |> AdventOfCode.Day02.part2()
    |> (&assert(&1 === 14)).()

    File.read!(file)
    |> String.split("\n", trim: true)
    |> AdventOfCode.Day02.part2()
    |> (&assert(&1 === 3_812_909)).()
  end
end
