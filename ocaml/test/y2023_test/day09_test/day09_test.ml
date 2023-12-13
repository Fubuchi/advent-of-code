open Alcotest
open Share_test
open Aoc_solution.Y2023.Day09

let input = [ "0 3 6 9 12 15"; "1 3 6 10 15 21"; "10 13 16 21 30 45" ]

let static_suites =
  [
    ("Expect: 114", `Quick, test int (fun () -> part_one input) 114);
    ("Expect: 2", `Quick, test int (fun () -> part_two input) 2);
  ]

let file_suites =
  read_file_suite "2023_09" (fun () ->
      Arg.read_arg "./y2023_test/day09_test/input.txt"
      |> Array.to_list
      |> fun input ->
      [
        ( "Expect:1904165718",
          `Quick,
          test int (fun () -> part_one input) 1904165718 );
        ("Expect:964", `Quick, test int (fun () -> part_two input) 964);
      ])

let suites = static_suites @ file_suites
