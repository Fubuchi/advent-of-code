open Alcotest
open Share_test
open Aoc_solution.Y2023.Day14

let input =
  [
    "O....#....";
    "O.OO#....#";
    ".....##...";
    "OO.#O....O";
    ".O.....O#.";
    "O.#..O.#.#";
    "..O..#O..O";
    ".......O..";
    "#....###..";
    "#OO..#....";
  ]

let static_suites =
  [
    ("Expect: 136", `Quick, test int (fun () -> part_one input) 136);
    ("Expect: 64", `Quick, test int (fun () -> part_two input) 64);
  ]

let file_suites =
  read_file_suite "2023_14" (fun () ->
      Arg.read_arg "./y2023_test/day14_test/input.txt"
      |> Array.to_list
      |> fun input ->
      [
        ("Expect: 113486", `Quick, test int (fun () -> part_one input) 113486);
        ("Expect: 104409", `Quick, test int (fun () -> part_two input) 104409);
      ])

let suites = static_suites @ file_suites
