open Containers
open Share.Data
open Share.Func

let parse input =
  let max_x = input |> List.hd |> String.length |> Fun.flip ( - ) 1 in
  let min_y = input |> List.length |> ( - ) 1 in
  let universal =
    input
    |> List.foldi
         (fun map y line ->
           line
           |> String.to_list
           |> List.foldi (fun m x c -> Int2Map.add (x, -y) c m) map)
         Int2Map.empty
  in

  let seq_x = range 0 max_x in
  let seq_y = range 0 ~step:(-1) min_y in
  let double_x =
    seq_x
    |> Seq.filter (fun x ->
           seq_y
           |> Seq.for_all (fun y ->
                  Stdlib.(Int2Map.find (x, y) universal = '.')))
    |> List.of_seq
  in

  let double_y =
    seq_y
    |> Seq.filter (fun y ->
           seq_x
           |> Seq.for_all (fun x ->
                  Stdlib.(Int2Map.find (x, y) universal = '.')))
    |> List.of_seq
  in
  (universal, double_x, double_y)

let sort_pair x y = (min x y, max x y)

let distance double_x double_y (x1, y1) (x2, y2) ~offset =
  let dis = abs (x1 - x2) + abs (y1 - y2) in
  let (x1, x2) = sort_pair x1 x2 in
  let extra_x = double_x |> List.count (fun x -> x >= x1 && x <= x2) in
  let (y1, y2) = sort_pair y1 y2 in
  let extra_y = double_y |> List.count (fun y -> y >= y1 && y <= y2) in
  dis + (extra_x * offset) + (extra_y * offset)

let solve input ~offset =
  let (universal, double_x, double_y) = parse input in
  universal
  |> Int2Map.filter (fun _ v -> Stdlib.(v = '#'))
  |> Int2Map.keys
  |> List.of_iter
  |> combnk 2
  |> List.map (function
       | [ p1; p2 ] -> distance double_x double_y p1 p2 ~offset
       | e -> unreachable Fmt.(Dump.list (Dump.pair int int)) e)
  |> List.fold_left ( + ) 0

let part_one = solve ~offset:1
let part_two ?(offset = 999999) = solve ~offset
