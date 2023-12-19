open Containers
open Share.Data
open Share.Func
module P = Share.Printer

let to_dir = function
  | "R" | "0" -> E
  | "D" | "1" -> S
  | "L" | "2" -> W
  | "U" | "3" -> N
  | e -> unreachable P.string e

let move (x, y) dis = function
  | E -> (x + dis, y)
  | W -> (x - dis, y)
  | N -> (x, y + dis)
  | S -> (x, y - dis)

let rec dig vertices count (x, y) = function
  | [] -> (vertices, count)
  | (dir, step) :: xs ->
      let next = move (x, y) step dir in
      dig (next :: vertices) (count + step) next xs

let rec shoelace_formula area = function
  | ((x1, y1), (x2, y2)) :: xs ->
      shoelace_formula (area + ((x1 * y2) - (x2 * y1))) xs
  | [] -> abs (area / 2)

let parse fmap input =
  input
  |> List.map (fun line ->
         line
         |> String.split_on_char ' '
         |> function
         | [ dir; step; color ] ->
             fmap (to_dir dir, Int.of_string_exn step, color)
         | e -> unreachable P.(lst string) e)

let solve input fmap =
  let (vertices, count) =
    input
    |> parse fmap
    |> dig [ (0, 0) ] 0 (0, 0)
    |> fun (v, c) ->
    (v |> windowed 2 1 ~discard:true |> List.map pair_of_list, c)
  in

  shoelace_formula 0 vertices + ((count / 2) + 1)

let part_one input = solve input (fun (dir, step, _) -> (dir, step))

let part_two input =
  solve input (fun (_, _, hex) ->
      let hex = hex |> String.drop 2 |> String.replace ~sub:")" ~by:"" in
      let length = String.length hex in
      let step = "0x" ^ String.take (length - 1) hex |> Int.of_string_exn in
      let dir = to_dir (String.drop (length - 1) hex) in
      (dir, step))
