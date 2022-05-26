defmodule AdventOfCode.Day02Test do
  use ExUnit.Case

  setup do
    [input: "test/day02/input.txt"]
  end

  @tag :skip
  test "part1", %{input: file} do
    TestHelper.multiple_lines(["2x3x4"], 58, &AdventOfCode.Day02.part1/1)
    TestHelper.multiple_lines(["1x1x10"], 43, &AdventOfCode.Day02.part1/1)
    TestHelper.multiple_lines(%{file: file}, 1_598_415, &AdventOfCode.Day02.part1/1)
  end

  @tag :skip
  test "part2", %{input: file} do
    TestHelper.multiple_lines(["2x3x4"], 34, &AdventOfCode.Day02.part2/1)
    TestHelper.multiple_lines(["1x1x10"], 14, &AdventOfCode.Day02.part2/1)
    TestHelper.multiple_lines(%{file: file}, 3_812_909, &AdventOfCode.Day02.part2/1)
  end
end
