open Alcotest
open Share_test
open Aoc_solution.Y2015.Day03

let static_suites =
  [
    ("> = 2", `Quick, test int part_one ">" 2);
    ("^>v< = 4", `Quick, test int part_one "^>v<" 4);
    ("^v^v^v^v^v = 2", `Quick, test int part_one "^v^v^v^v^v" 2);
    ("^v = 3", `Quick, test int part_two "^v" 3);
    ("^>v< = 3", `Quick, test int part_two "^>v<" 3);
    ("^v^v^v^v^v = 11", `Quick, test int part_two "^v^v^v^v^v" 11);
  ]

let file_suites =
  read_file_suite "2015_03" (fun () ->
      Arg.read_arg "./y2015_test/day03_test/input.txt"
      |> fun args ->
      args.(0)
      |> fun input ->
      [
        ("Expect 2081", `Quick, test int part_one input 2081);
        ("Expect 2341", `Quick, test int part_two input 2341);
      ])

let suites = static_suites @ file_suites
