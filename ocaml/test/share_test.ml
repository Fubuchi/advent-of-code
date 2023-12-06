open Alcotest

let test result_type part input expected () =
  let actual = part input in
  check result_type "Same value" expected actual

let read_file_suite suite read =
  Sys.argv
  |> Array.map (String.split_on_char '=')
  |> Array.find_map (function
       | [ "--suite"; v ] when v = suite || v = "all" -> Some v
       | _ -> None)
  |> function
  | Some _ -> read ()
  | _ -> []
