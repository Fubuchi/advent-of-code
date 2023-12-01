open Alcotest
open Share_test
open Aoc_solution.Y2015.Day05

let static_suites =
  [
    ("ugknbfddgicrmopn = 1", `Quick, test int part_one [ "ugknbfddgicrmopn" ] 1);
    ("aaa = 1", `Quick, test int part_one [ "aaa" ] 1);
    ("jchzalrnumimnmhp = 0", `Quick, test int part_one [ "jchzalrnumimnmhp" ] 0);
    ("haegwjzuvuyypxyu = 0", `Quick, test int part_one [ "haegwjzuvuyypxyu" ] 0);
    ("dvszwmarrgswjxmb = 0", `Quick, test int part_one [ "dvszwmarrgswjxmb" ] 0);
    ("qjhvhtzxzqqjkmpb = 1", `Quick, test int part_two [ "qjhvhtzxzqqjkmpb" ] 1);
    ("xxyxx = 1", `Quick, test int part_two [ "xxyxx" ] 1);
    ("uurcxstgmygtbstg = 0", `Quick, test int part_two [ "uurcxstgmygtbstg" ] 0);
    ("ieodomkazucvgmuy = 0", `Quick, test int part_two [ "ieodomkazucvgmuy" ] 0);
  ]

let file_suites =
  Arg.read_arg "./y2015_test/day05_test/input.txt"
  |> Array.to_list
  |> fun input ->
  [
    ("Expect 238", `Quick, test int part_one input 238);
    ("Expect 69", `Quick, test int part_two input 69);
  ]

let suites = static_suites @ file_suites
