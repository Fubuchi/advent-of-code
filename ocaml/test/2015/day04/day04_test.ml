open Alcotest
open Share_test
open Aoc_solution.Y2015.Day04

let static_suites =
  [
    ("abcdef = 609043", `Quick, test int part_one "abcdef" 609043);
    ("pqrstuv = 1048970", `Quick, test int part_one "pqrstuv" 1048970);
  ]

let file_suites =
  Arg.read_arg "./2015/day04/input.txt"
  |> fun args ->
  args.(0)
  |> fun input ->
  [
    ("Expect 282749", `Quick, test int part_one input 282749);
    ("Expect 9962624", `Quick, test int part_two input 9962624);
  ]

let suites = static_suites @ file_suites