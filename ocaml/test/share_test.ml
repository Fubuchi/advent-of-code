open Alcotest

let test result_type part input expected () =
  let actual = part input in
  check result_type "Same value" expected actual
