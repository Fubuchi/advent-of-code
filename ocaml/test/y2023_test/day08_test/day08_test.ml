open Alcotest
open Share_test
open Aoc_solution.Y2023.Day08

let input_1 =
  [
    "RL";
    "";
    "AAA = (BBB, CCC)";
    "BBB = (DDD, EEE)";
    "CCC = (ZZZ, GGG)";
    "DDD = (DDD, DDD)";
    "EEE = (EEE, EEE)";
    "GGG = (GGG, GGG)";
    "ZZZ = (ZZZ, ZZZ)";
  ]

let input_2 =
  [ "LLR"; ""; "AAA = (BBB, BBB)"; "BBB = (AAA, ZZZ)"; "ZZZ = (ZZZ, ZZZ)" ]

let input_3 =
  [
    "LR";
    "";
    "11A = (11B, XXX)";
    "11B = (XXX, 11Z)";
    "11Z = (11B, XXX)";
    "22A = (22B, XXX)";
    "22B = (22C, 22C)";
    "22C = (22Z, 22Z)";
    "22Z = (22B, 22B)";
    "XXX = (XXX, XXX)";
  ]

let static_suites =
  [
    ("Expect: 2", `Quick, test int (fun () -> part_one input_1) 2);
    ("Expect: 6", `Quick, test int (fun () -> part_one input_2) 6);
    ("Expect: 6", `Quick, test int (fun () -> part_two input_3) 6);
  ]

let file_suites =
  read_file_suite "2023_08" (fun () ->
      Arg.read_arg "./y2023_test/day08_test/input.txt"
      |> Array.to_list
      |> fun input ->
      [
        ("Expect = 15989", `Quick, test int (fun () -> part_one input) 15989);
        ( "Expect = 13830919117339",
          `Quick,
          test int (fun () -> part_two input) 13830919117339 );
      ])

let suites = static_suites @ file_suites
