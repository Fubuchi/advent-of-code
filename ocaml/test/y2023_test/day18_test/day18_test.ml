open Alcotest
open Share_test
open Aoc_solution.Y2023.Day18

let input =
  [
    "R 6 (#70c710)";
    "D 5 (#0dc571)";
    "L 2 (#5713f0)";
    "D 2 (#d2c081)";
    "R 2 (#59c680)";
    "D 2 (#411b91)";
    "L 5 (#8ceee2)";
    "U 2 (#caa173)";
    "L 1 (#1b58a2)";
    "U 2 (#caa171)";
    "R 2 (#7807d2)";
    "U 3 (#a77fa3)";
    "L 2 (#015232)";
    "U 2 (#7a21e3)";
  ]

let static_suites =
  [
    ("Expect: 62", `Quick, test int (fun () -> part_one input) 62);
    ( "Expect: 952408144115",
      `Quick,
      test int (fun () -> part_two input) 952408144115 );
  ]

let file_suites =
  read_file_suite "2023_18" (fun () ->
      Arg.read_arg "./y2023_test/day18_test/input.txt"
      |> Array.to_list
      |> fun input ->
      [
        ("Expect: 62500", `Quick, test int (fun () -> part_one input) 62500);
        ( "Expect: 122109860712709",
          `Quick,
          test int (fun () -> part_two input) 122109860712709 );
      ])

let suites = static_suites @ file_suites
