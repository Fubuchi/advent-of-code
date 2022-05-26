defmodule AdventOfCode.Day01Test do
  use ExUnit.Case

  setup do
    [input: "test/day01/input.txt"]
  end

  @tag :skip
  test "part1", %{input: file} do
    TestHelper.one_line_and_split("(())", 0, &AdventOfCode.Day01.part1/1)
    TestHelper.one_line_and_split(")())())", -3, &AdventOfCode.Day01.part1/1)
    TestHelper.one_line_and_split(%{file: file}, 232, &AdventOfCode.Day01.part1/1)
  end

  @tag :skip
  test "part2", %{input: file} do
    TestHelper.one_line_and_split(")", 1, &AdventOfCode.Day01.part2/1)
    TestHelper.one_line_and_split("()())", 5, &AdventOfCode.Day01.part2/1)
    TestHelper.one_line_and_split(%{file: file}, 1783, &AdventOfCode.Day01.part2/1)
  end
end
