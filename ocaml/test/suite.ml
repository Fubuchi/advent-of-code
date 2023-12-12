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

let suites_2023 =
  [
    ("2023_01", Y2023_test.Day01_test.suites);
    ("2023_02", Y2023_test.Day02_test.suites);
    ("2023_03", Y2023_test.Day03_test.suites);
    ("2023_04", Y2023_test.Day04_test.suites);
    ("2023_05", Y2023_test.Day05_test.suites);
    ("2023_06", Y2023_test.Day06_test.suites);
    ("2023_07", Y2023_test.Day07_test.suites);
    ("2023_08", Y2023_test.Day08_test.suites);
    ("2023_09", Y2023_test.Day09_test.suites);
    ("2023_10", Y2023_test.Day10_test.suites);
    ("2023_11", Y2023_test.Day11_test.suites);
    ("2023_12", Y2023_test.Day12_test.suites);
  ]

let suites =
  StringMap.of_list suites_2015 |> Fun.flip StringMap.add_list suites_2023
