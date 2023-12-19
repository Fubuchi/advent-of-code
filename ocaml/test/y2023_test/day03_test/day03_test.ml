open Alcotest
open Share_test
open Aoc_solution.Y2023.Day03

let static =
  [
    "467..114..";
    "...*......";
    "..35..633.";
    "......#...";
    "617*......";
    ".....+.58.";
    "..592.....";
    "......755.";
    "...$.*....";
    ".664.598..";
  ]

let static_suites =
  [
    ("Expect: 4361", `Quick, test int (fun () -> part_one static) 4361);
    ("Expect: 467835", `Quick, test int (fun () -> part_two static) 467835);
  ]

let file_suites =
  read_file_suite "2023_03" (fun () ->
      Arg.read_arg "./y2023_test/day03_test/input.txt"
      |> Array.to_list
      |> fun input ->
      [
        ("Expect: 527369", `Quick, test int (fun () -> part_one input) 527369);
        ( "Expect: 73074886",
          `Quick,
          test int (fun () -> part_two input) 73074886 );
      ])

let suites = static_suites @ file_suites
