defmodule AdventOfCode.Day04Test do
  use ExUnit.Case

  @tag :skip
  test "part 1" do
    TestHelper.one_line("abcdef", 609_043, &AdventOfCode.Day04.part1/1)
    TestHelper.one_line("pqrstuv", 1_048_970, &AdventOfCode.Day04.part1/1)
    TestHelper.one_line("yzbqklnj", 282_749, &AdventOfCode.Day04.part1/1)
  end

  @tag :skip
  test "part 2" do
    TestHelper.one_line("abcdef", 6_742_839, &AdventOfCode.Day04.part2/1)
    TestHelper.one_line("pqrstuv", 5_714_438, &AdventOfCode.Day04.part2/1)
    TestHelper.one_line("yzbqklnj", 9_962_624, &AdventOfCode.Day04.part2/1)
  end
end
