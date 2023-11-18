open Alcotest
open Share_test
open Aoc_solution.Y2015.Day02

let static_suites =
  [
    ("2x3x4 = 58", `Quick, test int part_one [ "2x3x4" ] 58);
    ("1x1x10 = 43", `Quick, test int part_one [ "1x1x10" ] 43);
    ("2x3x4 = 34", `Quick, test int part_two [ "2x3x4" ] 34);
    ("1x1x10 = 14", `Quick, test int part_two [ "1x1x10" ] 14);
  ]

let file_suites =
  Arg.read_arg "./2015/day02/input.txt"
  |> Array.to_list
  |> fun input ->
  [
    ("Expect 1598415", `Quick, test int part_one input 1598415);
    ("Expect 3812909", `Quick, test int part_two input 3812909);
  ]

let suites = static_suites @ file_suites
