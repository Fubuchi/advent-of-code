open Aoc_solution.Share.Func
open Aoc_solution.Share.Data

let () =
  let chosen_suite =
    Sys.argv
    |> Array.map (String.split_on_char '=')
    |> Array.find_map (function
         | [ "--suite"; v ] -> Some v
         | _ -> None)
    |> opt_get
  in

  (match chosen_suite with
  | "all" -> StringMap.bindings Suite.suites
  | suite -> [ (suite, StringMap.find suite Suite.suites) ])
  |> List.iter (fun (suite_name, suites) ->
         Alcotest.run suite_name ~argv:[| "_"; "-q" |] [ (suite_name, suites) ])
