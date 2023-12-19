open Alcotest
open Share_test
open Aoc_solution.Y2023.Day16

let input =
  [
    ".|...\\....";
    "|.-.\\.....";
    ".....|-...";
    "........|.";
    "..........";
    ".........\\";
    "..../.\\\\..";
    ".-.-/..|..";
    ".|....-|.\\";
    "..//.|....";
  ]

let static_suites =
  [
    ("Expect: 46", `Quick, test int (fun () -> part_one input) 46);
    ("Expect: 51", `Quick, test int (fun () -> part_two input) 51);
  ]

let file_suites =
  read_file_suite "2023_16" (fun () ->
      Arg.read_arg "./y2023_test/day16_test/input.txt"
      |> Array.to_list
      |> fun input ->
      [
        ("Expect: 7562", `Quick, test int (fun () -> part_one input) 7562);
        ("Expect: 7793", `Quick, test int (fun () -> part_two input) 7793);
      ])

let suites = static_suites @ file_suites
