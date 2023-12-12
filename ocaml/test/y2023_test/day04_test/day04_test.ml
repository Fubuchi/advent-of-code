open Alcotest
open Share_test
open Aoc_solution.Y2023.Day04

let static =
  [
    "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53";
    "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19";
    "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1";
    "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83";
    "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36";
    "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11";
  ]

let static_suites =
  [
    ("Expect: 13", `Quick, test int (fun () -> part_one static) 13);
    ("Expect: 30", `Quick, test int (fun () -> part_two static) 30);
  ]

let file_suites =
  read_file_suite "2023_04" (fun () ->
      Arg.read_arg "./y2023_test/day04_test/input.txt"
      |> Array.to_list
      |> fun input ->
      [
        ("Expect = 24542", `Quick, test int (fun () -> part_one input) 24542);
        ("Expect = 8736438", `Quick, test int (fun () -> part_two input) 8736438);
      ])

let suites = static_suites @ file_suites
