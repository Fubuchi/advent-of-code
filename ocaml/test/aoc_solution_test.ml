open Aoc_solution.Share

let suites =
  StringMap.of_list
    [
      ("2015_01", Y2015_test.Day01_test.suites);
      ("2015_02", Y2015_test.Day02_test.suites);
    ]

let () =
  let chosen_suite =
    Sys.argv
    |> Array.to_list
    |> BatList.drop 1
    |> List.map (String.split_on_char '=')
    |> List.find (function
         | [ "--suite"; _ ] -> true
         | _ -> false)
    |> BatList.last
  in

  suites
  |> StringMap.find chosen_suite
  |> fun suite ->
  [ (chosen_suite, suite) ] |> Alcotest.run chosen_suite ~argv:[| "_,"; "-q" |]
