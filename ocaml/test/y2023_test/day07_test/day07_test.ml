open Alcotest
open Share_test
open Aoc_solution.Y2023.Day07

let static = [ "32T3K 765"; "T55J5 684"; "KK677 28"; "KTJJT 220"; "QQQJA 483" ]

let static_suites =
  [
    ("Expect: 6440", `Quick, test int part_one static 6440);
    ("Expect: 5905", `Quick, test int part_two static 5905);
  ]

let file_suites =
  read_file_suite "2023_07" (fun () ->
      Arg.read_arg "./y2023_test/day07_test/input.txt"
      |> Array.to_list
      |> fun input ->
      [
        ("Expect = 250254244", `Quick, test int part_one input 250254244);
        ("Expect = 250087440", `Quick, test int part_two input 250087440);
      ])

let suites = static_suites @ file_suites
