open Alcotest
open Share_test
open Aoc_solution.Y2015.Day01

let static_suites =
  [
    ("(()) = 0", `Quick, test int part_one "(())" 0);
    ("(()(()( = 3", `Quick, test int part_one "(()(()(" 3);
    (")())()) = -3", `Quick, test int part_one ")())())" (-3));
    (") = 1", `Quick, test int part_two ")" 1);
    ("()()) = 5", `Quick, test int part_two "()())" 5);
  ]

let file_suites =
  read_file_suite "2015_01" (fun () ->
      Arg.read_arg "./y2015_test/day01_test/input.txt"
      |> fun args ->
      args.(0)
      |> fun input ->
      [
        (Format.sprintf "%s = 232" input, `Quick, test int part_one input 232);
        (Format.sprintf "%s = 1783" input, `Quick, test int part_two input 1783);
      ])

let suites = static_suites @ file_suites
