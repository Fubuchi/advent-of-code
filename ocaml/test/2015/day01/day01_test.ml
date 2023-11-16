open Alcotest
open Aoc_solution.Y2015.Day01

let test part input expected () =
  let result = part input in
  check int "Same value" result expected

let static_suites =
  [
    ("(()) = 0", `Quick, test part_one "(())" 0);
    ("(()(()( = 3", `Quick, test part_one "(()(()(" 3);
    (")())()) = -3", `Quick, test part_one ")())())" (-3));
    (") = 1", `Quick, test part_two ")" 1);
    ("()()) = 5", `Quick, test part_two "()())" 5);
  ]

let file_suites =
  Arg.read_arg "./2015/day01/input.txt"
  |> fun args ->
  args.(0)
  |> fun input ->
  [
    (Format.sprintf "%s = 232" input, `Quick, test part_one input 232);
    (Format.sprintf "%s = 1783" input, `Quick, test part_two input 1783);
  ]

let suites = static_suites @ file_suites
