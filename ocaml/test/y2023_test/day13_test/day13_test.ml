open Alcotest
open Share_test
open Aoc_solution.Y2023.Day13

let input =
  [
    "#.##..##.";
    "..#.##.#.";
    "##......#";
    "##......#";
    "..#.##.#.";
    "..##..##.";
    "#.#.##.#.";
    "";
    "#...##..#";
    "#....#..#";
    "..##..###";
    "#####.##.";
    "#####.##.";
    "..##..###";
    "#....#..#";
  ]

let static_suites =
  [
    ("Expect: 405", `Quick, test int (fun () -> part_one input) 405);
    ("Expect: 400", `Quick, test int (fun () -> part_two input) 400);
  ]

let file_suites =
  read_file_suite "2023_13" (fun () ->
      Arg.read_arg "./y2023_test/day13_test/input.txt"
      |> Array.to_list
      |> fun input ->
      [
        ("Expect: 33975", `Quick, test int (fun () -> part_one input) 33975);
        ("Expect: 29083", `Quick, test int (fun () -> part_two input) 29083);
      ])

let suites = static_suites @ file_suites
