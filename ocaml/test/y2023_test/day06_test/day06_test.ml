open Alcotest
open Share_test
open Aoc_solution.Y2023.Day06

let static = [ "Time:      7  15   30"; "Distance:  9  40  200" ]

let static_suites =
  [
    ("Expect: 288", `Quick, test int (fun () -> part_one static) 288);
    ("Expect: 71503", `Quick, test int (fun () -> part_two static) 71503);
  ]

let file_suites =
  read_file_suite "2023_06" (fun () ->
      Arg.read_arg "./y2023_test/day06_test/input.txt"
      |> Array.to_list
      |> fun input ->
      [
        ("Expect: 2374848", `Quick, test int (fun () -> part_one input) 2374848);
        ( "Expect: 39132886",
          `Quick,
          test int (fun () -> part_two input) 39132886 );
      ])

let suites = static_suites @ file_suites
