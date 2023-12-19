open Containers
open Share.Data
open Share.Func
open Share.Operator
module P = Share.Printer

let parse_grid = parse_grid ~fvalue:Fun.id

let roll_north seq_x seq_y map =
  seq_x
  |> Seq.fold
       (fun map x ->
         seq_y
         |> Seq.drop 1
         |> Seq.fold
              (fun map y ->
                match Int2Map.get (x, y) map with
                | Some '#' | None -> map
                | Some 'O' ->
                    let roll_length =
                      range (y + 1) 0
                      |> Seq.take_while (fun y -> not <| Int2Map.mem (x, y) map)
                      |> Seq.length
                    in
                    map
                    |> Int2Map.remove (x, y)
                    |> Int2Map.add (x, y + roll_length) 'O'
                | e -> unreachable P.(option char) e)
              map)
       map

let roll_west seq_x seq_y map =
  seq_y
  |> Seq.fold
       (fun map y ->
         seq_x
         |> Seq.drop 1
         |> Seq.fold
              (fun map x ->
                match Int2Map.get (x, y) map with
                | Some '#' | None -> map
                | Some 'O' ->
                    let roll_length =
                      range (x - 1) 0 ~step:(-1)
                      |> Seq.take_while (fun x -> not <| Int2Map.mem (x, y) map)
                      |> Seq.length
                    in
                    map
                    |> Int2Map.remove (x, y)
                    |> Int2Map.add (x - roll_length, y) 'O'
                | e -> unreachable P.(option char) e)
              map)
       map

let roll_south seq_x rev_seq_y min_y map =
  seq_x
  |> Seq.fold
       (fun map x ->
         rev_seq_y
         |> Seq.drop 1
         |> Seq.fold
              (fun map y ->
                match Int2Map.get (x, y) map with
                | Some '#' | None -> map
                | Some 'O' ->
                    let roll_length =
                      range (y - 1) min_y ~step:(-1)
                      |> Seq.take_while (fun y -> not <| Int2Map.mem (x, y) map)
                      |> Seq.length
                    in
                    map
                    |> Int2Map.remove (x, y)
                    |> Int2Map.add (x, y - roll_length) 'O'
                | e -> unreachable P.(option char) e)
              map)
       map

let roll_east rev_seq_x seq_y max_x map =
  seq_y
  |> Seq.fold
       (fun map y ->
         rev_seq_x
         |> Seq.drop 1
         |> Seq.fold
              (fun map x ->
                match Int2Map.get (x, y) map with
                | Some '#' | None -> map
                | Some 'O' ->
                    let roll_length =
                      range (x + 1) max_x
                      |> Seq.take_while (fun x -> not <| Int2Map.mem (x, y) map)
                      |> Seq.length
                    in
                    map
                    |> Int2Map.remove (x, y)
                    |> Int2Map.add (x + roll_length, y) 'O'
                | e -> unreachable P.(option char) e)
              map)
       map

let calculate_load min_y map =
  map
  |> Int2Map.filter (fun _ v -> Stdlib.(v = 'O'))
  |> Int2Map.keys
  |> Iter.fold (fun load (_, y) -> load + (abs (min_y - y) + 1)) 0

let cycle map seq_x seq_y max_x min_y ~max_cycle =
  let rev_seq_x = seq_x |> Seq.to_rev_list |> List.to_seq in
  let rev_seq_y = seq_y |> Seq.to_rev_list |> List.to_seq in
  let calculate_load = calculate_load min_y in

  range 1 max_cycle
  |> Seq.fold_left
       (fun (map, load, seen, repeat_at) i ->
         match repeat_at with
         | Some _ -> (map, load, seen, repeat_at)
         | _ ->
             let map = roll_north seq_x seq_y map in
             let north_load = calculate_load map in
             let map = roll_west seq_x seq_y map in
             let west_load = calculate_load map in
             let map = roll_south seq_x rev_seq_y min_y map in
             let south_load = calculate_load map in
             let map = roll_east rev_seq_x seq_y max_x map in
             let east_load = calculate_load map in
             let pattern = (north_load, west_load, south_load, east_load) in

             if Int4Map.mem pattern seen then
               (map, load, seen, Some (pattern, i))
             else
               ( map,
                 IntMap.add i east_load load,
                 Int4Map.add pattern i seen,
                 None ))
       (map, IntMap.singleton 0 (calculate_load map), Int4Map.empty, None)
  |> fun (_, load, seen, repeat_at) ->
  match repeat_at with
  | None ->
      P.failwith "Can not find pattern with %d cycle(s), try bigger number"
        max_cycle
  | Some i -> (i, seen, load)

let part_one input =
  let map =
    input |> parse_grid |> Int2Map.filter (fun _ v -> Stdlib.(v <> '.'))
  in
  let (max_x, min_y) = maxx_miny input in
  let seq_x = range 0 max_x in
  let seq_y = range 0 ~step:(-1) min_y in
  map |> roll_north seq_x seq_y |> calculate_load min_y

let part_two input =
  let map =
    input |> parse_grid |> Int2Map.filter (fun _ v -> Stdlib.(v <> '.'))
  in
  let (max_x, min_y) = maxx_miny input in
  let seq_x = range 0 max_x in
  let seq_y = range 0 ~step:(-1) min_y in
  let ((pattern, repeat_at), seen, load) =
    cycle map seq_x seq_y max_x min_y ~max_cycle:1000
  in
  let start_at = Int4Map.find pattern seen in
  let cycle_length = repeat_at - start_at in
  let final_index = start_at + ((1_000_000_000 - start_at) mod cycle_length) in
  IntMap.find final_index load
