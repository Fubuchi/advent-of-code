open Alcotest
open Share_test
open Aoc_solution.Y2015.Day04

let static_suites =
  [
    ("abcdef = 609043", `Quick, test int (fun () -> part_one "abcdef") 609043);
    ( "pqrstuv = 1048970",
      `Quick,
      test int (fun () -> part_one "pqrstuv") 1048970 );
  ]

let file_suites =
  read_file_suite "2015_04" (fun () ->
      Arg.read_arg "./y2015_test/day04_test/input.txt"
      |> fun args ->
      args.(0)
      |> fun input ->
      [
        ("Expect 282749", `Quick, test int (fun () -> part_one input) 282749);
        ("Expect 9962624", `Quick, test int (fun () -> part_two input) 9962624);
      ])

let suites = static_suites @ file_suites
