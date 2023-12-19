open Alcotest
open Share_test
open Aoc_solution.Y2023.Day17

let input =
  [
    "2413432311323";
    "3215453535623";
    "3255245654254";
    "3446585845452";
    "4546657867536";
    "1438598798454";
    "4457876987766";
    "3637877979653";
    "4654967986887";
    "4564679986453";
    "1224686865563";
    "2546548887735";
    "4322674655533";
  ]

let static_suites =
  [
    ("Expect: 102", `Quick, test int (fun () -> part_one input) 102);
    ("Expect: 94", `Quick, test int (fun () -> part_two input) 94);
  ]

let file_suites =
  read_file_suite "2023_17" (fun () ->
      Arg.read_arg "./y2023_test/day17_test/input.txt"
      |> Array.to_list
      |> fun input ->
      [
        ("Expect: 1044", `Quick, test int (fun () -> part_one input) 1044);
        ("Expect: 1227", `Quick, test int (fun () -> part_two input) 1227);
      ])

let suites = static_suites @ file_suites
