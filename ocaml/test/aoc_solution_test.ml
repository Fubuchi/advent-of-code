open Aoc_solution.Share.Data

let () =
  let chosen_suite =
    Sys.argv
    |> Array.map (String.split_on_char '=')
    |> Array.find_map (function
         | [ "--suite"; v ] -> Some v
         | _ -> None)
    |> Option.get
  in

  Suite.suites
  |> StringMap.find chosen_suite
  |> fun suite ->
  [ (chosen_suite, suite) ] |> Alcotest.run chosen_suite ~argv:[| "_"; "-q" |]
