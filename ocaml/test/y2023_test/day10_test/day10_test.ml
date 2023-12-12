open Alcotest
open Share_test
open Aoc_solution.Y2023.Day10

let input_1 = [ "..F7."; ".FJ|."; "SJ.L7"; "|F--J"; "LJ..." ]

let input_2 =
  [
    "FF7FSF7F7F7F7F7F---7";
    "L|LJ||||||||||||F--J";
    "FL-7LJLJ||||||LJL-77";
    "F--JF--7||LJLJ7F7FJ-";
    "L---JF-JLJ.||-FJLJJ7";
    "|F|F-JF---7F7-L7L|7|";
    "|FFJF7L7F-JF7|JL---7";
    "7-L-JL7||F7|L7F-7F7|";
    "L.L7LFJ|||||FJL7||LJ";
    "L7JLJL-JLJLJL--JLJ.L";
  ]

let static_suites =
  [
    ("Expect: 8", `Quick, test int (fun () -> part_one input_1) 8);
    ("Expect: 10", `Quick, test int (fun () -> part_two input_2) 10);
  ]

let file_suites =
  read_file_suite "2023_10" (fun () ->
      Arg.read_arg "./y2023_test/day10_test/input.txt"
      |> Array.to_list
      |> fun input ->
      [
        ("Expect = 6640", `Quick, test int (fun () -> part_one input) 6640);
        ("Expect = 411", `Quick, test int (fun () -> part_two input) 411);
      ])

let suites = static_suites @ file_suites
