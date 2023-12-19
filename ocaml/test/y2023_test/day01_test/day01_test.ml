open Alcotest
open Share_test
open Aoc_solution.Y2023.Day01

let fist = [ "1abc2"; "pqr3stu8vwx"; "a1b2c3d4e5f"; "treb7uchet" ]

let second =
  [
    "two1nine";
    "eightwothree";
    "abcone2threexyz";
    "xtwone3four";
    "4nineeightseven2";
    "zoneight234";
    "7pqrstsixteen";
  ]

let static_suites =
  [
    ("Expect: 142", `Quick, test int (fun () -> part_one fist) 142);
    ("Expect: 281", `Quick, test int (fun () -> part_two second) 281);
  ]

let file_suites =
  read_file_suite "2023_01" (fun () ->
      Arg.read_arg "./y2023_test/day01_test/input.txt"
      |> Array.to_list
      |> fun input ->
      [
        ("Expect: 56465", `Quick, test int (fun () -> part_one input) 56465);
        ("Expect: 55902", `Quick, test int (fun () -> part_two input) 55902);
      ])

let suites = static_suites @ file_suites
