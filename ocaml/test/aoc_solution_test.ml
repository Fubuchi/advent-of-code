open Containers
open Aoc_solution.Share.Func
open Aoc_solution.Share.Data

let () =
  let chosen_suite =
    Sys.argv
    |> Array.map (String.split_on_char '=')
    |> Array.find_map (function
         | [ "--suite"; v ] -> Some (String.split ~by:"," v)
         | _ -> None)
    |> opt_get
  in

  (match chosen_suite with
  | [ "all" ] -> StringMap.bindings Suite.suites
  | suites ->
      suites
      |> List.map (fun suite -> (suite, StringMap.find suite Suite.suites)))
  |> Alcotest.run "AOC test" ~argv:[| "_"; "-q" |]
