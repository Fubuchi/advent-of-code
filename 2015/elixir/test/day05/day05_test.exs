defmodule AdventOfCode.Day05Test do
  use ExUnit.Case

  setup do
    [input: "test/day05/input.txt"]
  end

  @tag :skip
  test "part 1", %{input: file} do
    TestHelper.multiple_lines(
      [
        "ugknbfddgicrmopn",
        "aaa",
        "jchzalrnumimnmhp",
        "haegwjzuvuyypxyu",
        "dvszwmarrgswjxmb"
      ],
      2,
      &AdventOfCode.Day05.part1/1
    )

    TestHelper.multiple_lines(%{file: file}, 238, &AdventOfCode.Day05.part1/1)
  end

  @tag :skip
  test "part 2", %{input: file} do
    TestHelper.multiple_lines(
      [
        "qjhvhtzxzqqjkmpb",
        "xxyxx",
        "uurcxstgmygtbstg",
        "ieodomkazucvgmuy"
      ],
      2,
      &AdventOfCode.Day05.part2/1
    )

    TestHelper.multiple_lines(%{file: file}, 69, &AdventOfCode.Day05.part2/1)
  end
end
