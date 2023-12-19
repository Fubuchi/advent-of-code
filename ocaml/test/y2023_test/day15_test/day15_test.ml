open Alcotest
open Share_test
open Aoc_solution.Y2023.Day15

let input = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

let static_suites =
  [
    ("Expect: 1320", `Quick, test int (fun () -> part_one input) 1320);
    ("Expect: 145", `Quick, test int (fun () -> part_two input) 145);
  ]

let file_suites =
  read_file_suite "2023_15" (fun () ->
      Arg.read_arg "./y2023_test/day15_test/input.txt"
      |> Array.to_list
      |> List.hd
      |> fun input ->
      [
        ("Expect: 519041", `Quick, test int (fun () -> part_one input) 519041);
        ("Expect: 260530", `Quick, test int (fun () -> part_two input) 260530);
      ])

let suites = static_suites @ file_suites
