open Alcotest
open Share_test
open Aoc_solution.Y2023.Day02

let static =
  [
    "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green";
    "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue";
    "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red";
    "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red";
    "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green";
  ]

let static_suites =
  [
    ("Expect: 8", `Quick, test int part_one static 8);
    ("Expect: 2286", `Quick, test int part_two static 2286);
  ]

let file_suites =
  read_file_suite "2023_02" (fun () ->
      Arg.read_arg "./y2023_test/day02_test/input.txt"
      |> Array.to_list
      |> fun input ->
      [
        ("Expect = 2105", `Quick, test int part_one input 2105);
        ("Expect = 72422", `Quick, test int part_two input 72422);
      ])

let suites = static_suites @ file_suites
