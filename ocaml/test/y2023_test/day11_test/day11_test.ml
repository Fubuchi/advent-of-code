open Alcotest
open Share_test
open Aoc_solution.Y2023.Day11

let input =
  [
    "...#......";
    ".......#..";
    "#.........";
    "..........";
    "......#...";
    ".#........";
    ".........#";
    "..........";
    ".......#..";
    "#...#.....";
  ]

let static_suites =
  [
    ("Expect: 374", `Quick, test int (fun () -> part_one input) 374);
    ("Expect: 1030", `Quick, test int (fun () -> part_two ~offset:9 input) 1030);
    ("Expect: 8410", `Quick, test int (fun () -> part_two ~offset:99 input) 8410);
  ]

let file_suites =
  read_file_suite "2023_11" (fun () ->
      Arg.read_arg "./y2023_test/day11_test/input.txt"
      |> Array.to_list
      |> fun input ->
      [
        ( "Expect = 10494813",
          `Quick,
          test int (fun () -> part_one input) 10494813 );
        ( "Expect = 840988812853",
          `Quick,
          test int (fun () -> part_two input) 840988812853 );
      ])

let suites = static_suites @ file_suites
