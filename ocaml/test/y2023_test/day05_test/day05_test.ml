open Alcotest
open Share_test
open Aoc_solution.Y2023.Day05

let static =
  [
    "seeds: 79 14 55 13";
    "";
    "seed-to-soil map:";
    "50 98 2";
    "52 50 48";
    "";
    "soil-to-fertilizer map:";
    "0 15 37";
    "37 52 2";
    "39 0 15";
    "";
    "fertilizer-to-water map:";
    "49 53 8";
    "0 11 42";
    "42 0 7";
    "57 7 4";
    "";
    "water-to-light map:";
    "88 18 7";
    "18 25 70";
    "";
    "light-to-temperature map:";
    "45 77 23";
    "81 45 19";
    "68 64 13";
    "";
    "temperature-to-humidity map:";
    "0 69 1";
    "1 0 69";
    "";
    "humidity-to-location map:";
    "60 56 37";
    "56 93 4";
  ]

let static_suites =
  [
    ("Expect: 35", `Quick, test int (fun () -> part_one static) 35);
    ("Expect: 46", `Quick, test int (fun () -> part_two static) 46);
  ]

let file_suites =
  read_file_suite "2023_05" (fun () ->
      Arg.read_arg "./y2023_test/day05_test/input.txt"
      |> Array.to_list
      |> fun input ->
      [
        ( "Expect: 836040384",
          `Quick,
          test int (fun () -> part_one input) 836040384 );
        ( "Expect: 10834440",
          `Quick,
          test int (fun () -> part_two input) 10834440 );
      ])

let suites = static_suites @ file_suites
