open Containers
open Share.Func
module P = Share.Printer

let re_main =
  Pcre.regexp
    "(?=(one|two|three|four|five|six|seven|eight|nine|1|2|3|4|5|6|7|8|9))\\w|\\d"

let re_digit = Pcre.regexp "[0-9]"

let to_int = function
  | "one" -> 1
  | "two" -> 2
  | "three" -> 3
  | "four" -> 4
  | "five" -> 5
  | "six" -> 6
  | "seven" -> 7
  | "eight" -> 8
  | "nine" -> 9
  | i -> Int.of_string_exn i

let is_digit = Pcre.pmatch ~rex:re_digit

let solve mapping input =
  input
  |> List.map (fun s -> Pcre.extract_all ~rex:re_main s |> mapping)
  |> List.map (function
       | [||] -> failwith "Empty match!"
       | [| x |] -> x * 11
       | a -> (a.(0) * 10) + a.(Array.length a - 1))
  |> List.fold_left ( + ) 0

let part_one input =
  input
  |> solve
       (Array.filter_map (function
         | [| _; n |] when is_digit n -> Some (to_int n)
         | [| _; _ |] -> None
         | a -> unreachable P.(arr string) a))

let part_two input =
  input
  |> solve
       (Array.map (function
         | [| _; n |] -> to_int n
         | a -> unreachable P.(arr string) a))
