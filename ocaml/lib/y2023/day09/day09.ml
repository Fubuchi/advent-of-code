open Containers
open Share.Func
open Share.Data
open Share.Operator
module P = Share.Printer

let parse_line line =
  line
  |> String.split_on_char ' '
  |> List.map Int.of_string_exn
  |> windowed 2 1 ~discard:true
  |> List.map pair_of_list

let rec next_line acc = function
  | [ (a, b) ] -> (b - a) :: acc |> List.rev
  | (a, b) :: xs -> next_line ((b - a) :: acc) xs
  | [] -> P.failwith "Invalid []"

let rec pascal_triangle = function
  | curr :: _ as lines ->
      let next_line = next_line [] curr in
      let next =
        (next_line |> windowed 2 1 ~discard:true |> List.map pair_of_list)
        :: lines
      in
      if next_line |> IntSet.of_list |> IntSet.cardinal |> ( = ) 1 then
        next
      else
        pascal_triangle next
  | [] -> P.failwith "Invalid []"

let sovle input fortune_teller =
  input
  |> List.map parse_line
  |> List.map (fun line -> pascal_triangle [ line ] |> fortune_teller)
  |> List.fold_left ( + ) 0

let part_one input =
  let predict_future lines =
    lines
    |> List.map (List.last_opt >> opt_get >> snd)
    |> List.reduce ( + )
    |> opt_get
  in

  sovle input predict_future

let part_two input =
  let find_past lines =
    lines
    |> List.map (List.hd >> fst)
    |> List.reduce (Fun.flip ( - ))
    |> opt_get
  in

  sovle input find_past
