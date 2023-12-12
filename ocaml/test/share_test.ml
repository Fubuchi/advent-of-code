open Containers
open Alcotest

let test result_type f expected () =
  let actual = f () in
  check result_type "Same value" expected actual

let read_file_suite suite read =
  Sys.argv
  |> Array.map (String.split_on_char '=')
  |> Array.find_map (function
       | [ "--suite"; v ] ->
           let suites = v |> String.split ~by:"," in
           if List.mem "all" suites || List.mem suite suites then
             Some suite
           else
             None
       | _ -> None)
  |> function
  | Some _ -> read ()
  | _ -> []
