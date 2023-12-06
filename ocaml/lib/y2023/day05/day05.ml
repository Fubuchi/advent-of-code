open Containers
open Share.Func
open Share.Operator
module P = Share.Printer

type mapper = {
  source : int;
  target : int;
  range : int;
}

let intersect fmap (x1, y1) (x2, y2) =
  (* x1--y1--x2--y2 or x2--y2--x1--y1 *)
  if y1 < x2 || x1 > y2 then
    [ (None, x1, y1) ]
  (* x2 <= x1--y1 <= y2 *)
  else if x2 <= x1 && y1 <= y2 then
    [ (Some fmap, x1, y1) ]
  (* x1 <= x2--y2 <= y1 *)
  else if x2 >= x1 && y2 <= y1 then
    let mid = [ (Some fmap, x2, y2) ] in
    let lelf =
      if x2 > x1 then
        [ (None, x1, x2 - 1) ]
      else
        []
    in
    let right =
      if y2 < y1 then
        [ (None, y2 + 1, y1) ]
      else
        []
    in
    lelf @ mid @ right
  (* x1--x2 <= y1--y2 *)
  else if x2 <= y1 && x2 > x1 then
    [ (None, x1, x2 - 1); (Some fmap, x2, y1) ]
  (* x2--x1 <= y2--y1 *)
  else
    [ (Some fmap, x1, y2); (None, y2 + 1, y1) ]

let map source target range num =
  match num with
  | num when num >= source && num < source + range ->
      let offset = num - source in
      target + offset
  | num -> unreachable P.(pair int int) (num, range)

let parse_mapper = function
  | name :: instructions ->
      instructions
      |> List.map (String.split_on_char ' ' >> List.map Int.of_string_exn)
      |> List.map (function
           | [ target; source; range ] -> { source; target; range }
           | e -> unreachable P.(lst int) e)
      |> fun mappers -> (name, mappers)
  | e -> unreachable P.(lst string) e

let process_mapping (x, y) mappers =
  mappers
  |> List.fold_left
       (fun (mapped, pending) mapper ->
         let (p, m) =
           pending
           |> List.flat_map (fun (x, y) ->
                  let { source; range; target } = mapper in
                  let fmap = map source target range in
                  intersect fmap (x, y) (source, source + range - 1))
           |> List.partition_map_either (function
                | (None, x, y) -> Left (x, y)
                | (Some f, x, y) -> Right (f x, f y))
         in
         (m @ mapped, p))
       ([], [ (x, y) ])
  |> fun (mapped, can_not_map) -> mapped @ can_not_map

let solve seeds mappers =
  seeds
  |> List.flat_map (fun seed ->
         mappers
         |> List.fold_left
              (fun seed_ranges (_, mappers) ->
                seed_ranges
                |> List.flat_map (fun seed_range ->
                       process_mapping seed_range mappers))
              [ seed ])
  |> List.sort (fun (x1, _) (x2, _) -> compare x1 x2)
  |> List.hd
  |> fst

let parse seed_parser input =
  input
  |> List.group_succ ~eq:(fun a b -> Stdlib.(a <> "" && b <> ""))
  |> List.filter (function
       | [ "" ] -> false
       | _ -> true)
  |> function
  | [ seeds ] :: mappers ->
      ( seed_parser seeds,
        mappers
        |> List.map (function
             | [] -> unreachable P.(lst (lst string)) mappers
             | maps -> parse_mapper maps) )
  | e -> unreachable P.(lst (lst string)) e

let part_one input =
  let seed_parser seeds =
    seeds
    |> String.replace ~sub:"seeds: " ~by:""
    |> String.split_on_char ' '
    |> List.map (fun seed -> Int.of_string_exn seed |> fun x -> (x, x))
  in
  parse seed_parser input ||> solve

let part_two input =
  let seed_parser seeds =
    seeds
    |> String.replace ~sub:"seeds: " ~by:""
    |> String.split_on_char ' '
    |> List.map Int.of_string_exn
    |> List.chunks 2
    |> List.map (function
         | [ x; offset ] -> (x, x + offset - 1)
         | e -> unreachable P.(lst int) e)
  in
  parse seed_parser input ||> solve
