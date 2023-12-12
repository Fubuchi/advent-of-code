open Alcotest
open Share_test
open Aoc_solution.Y2023.Day12

let input =
  [
    "???.### 1,1,3";
    ".??..??...?##. 1,1,3";
    "?#?#?#?#?#?#?#? 1,3,1,6";
    "????.#...#... 4,1,1";
    "????.######..#####. 1,6,5";
    "?###???????? 3,2,1";
  ]

let static_suites =
  [
    ("Expect: 21", `Quick, test int (fun () -> part_one input) 21);
    ("Expect: 525152", `Quick, test int (fun () -> part_two input) 525152);
  ]

let file_suites =
  read_file_suite "2023_12" (fun () ->
      Arg.read_arg "./y2023_test/day12_test/input.txt"
      |> Array.to_list
      |> fun input ->
      [
        ("Expect = 7694", `Quick, test int (fun () -> part_one input) 7694);
        ( "Expect = 5071883216318",
          `Quick,
          test int (fun () -> part_two input) 5071883216318 );
      ])

let suites = static_suites @ file_suites
