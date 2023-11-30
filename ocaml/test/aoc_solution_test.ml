open Aoc_solution.Share.Data

let suites =
  StringMap.of_list
    [
      ("2015_01", Y2015_test.Day01_test.suites);
      ("2015_02", Y2015_test.Day02_test.suites);
      ("2015_03", Y2015_test.Day03_test.suites);
      ("2015_04", Y2015_test.Day04_test.suites);
      ("2015_05", Y2015_test.Day05_test.suites);
    ]

let () =
  let chosen_suite =
    Sys.argv
    |> Array.map (String.split_on_char '=')
    |> Array.find_map (function
         | [ "--suite"; v ] -> Some v
         | _ -> None)
    |> Option.get
  in

  suites
  |> StringMap.find chosen_suite
  |> fun suite ->
  [ (chosen_suite, suite) ] |> Alcotest.run chosen_suite ~argv:[| "_"; "-q" |]
