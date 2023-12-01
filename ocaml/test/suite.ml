open Aoc_solution.Share.Data

let suites_2015 =
  [
    ("2015_01", Y2015_test.Day01_test.suites);
    ("2015_02", Y2015_test.Day02_test.suites);
    ("2015_03", Y2015_test.Day03_test.suites);
    ("2015_04", Y2015_test.Day04_test.suites);
    ("2015_05", Y2015_test.Day05_test.suites);
    ("2015_05", Y2015_test.Day05_test.suites);
  ]

let suites_2023 = [ ("2023_01", Y2023_test.Day01_test.suites) ]

let suites =
  StringMap.of_list suites_2015 |> Fun.flip StringMap.add_list suites_2023
