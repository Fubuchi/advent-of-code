defmodule AdventOfCode.Day03Test do
  use ExUnit.Case

  setup do
    [input: "test/day03/input.txt"]
  end

  @tag :skip
  test "part1", %{input: file} do
    TestHelper.one_line_and_split("^>v<", 4, &AdventOfCode.Day03.part1/1)
    TestHelper.one_line_and_split("^v^v^v^v^v", 2, &AdventOfCode.Day03.part1/1)
    TestHelper.one_line_and_split(%{file: file}, 2081, &AdventOfCode.Day03.part1/1)
  end

  @tag :skip
  test "part2", %{input: file} do
    TestHelper.one_line_and_split("^>v<", 3, &AdventOfCode.Day03.part2/1)
    TestHelper.one_line_and_split("^v^v^v^v^v", 11, &AdventOfCode.Day03.part2/1)
    TestHelper.one_line_and_split(%{file: file}, 2341, &AdventOfCode.Day03.part2/1)
  end
end
