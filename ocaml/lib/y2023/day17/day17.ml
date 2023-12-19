open Containers
open Share.Data
open Share.Operator
open Share.Func
open Share.Ext
module P = Share.Printer

module PQ =
  Psq.Make
    (struct
      type t = int * int * int * diection

      let compare = Stdlib.compare
    end)
    (Int)

let go map final min_step max_step =
  let cost = Hashtbl.create 16 in

  let next_vector = function
    | S -> [ (S, (0, -1)); (W, (-1, 0)); (E, (1, 0)) ]
    | N -> [ (N, (0, 1)); (W, (-1, 0)); (E, (1, 0)) ]
    | E -> [ (E, (1, 0)); (S, (0, -1)); (N, (0, 1)) ]
    | W -> [ (W, (-1, 0)); (S, (0, -1)); (N, (0, 1)) ]
  in

  let back_track = function
    | N -> fun (x, y) -> (x, y - 1)
    | S -> fun (x, y) -> (x, y + 1)
    | W -> fun (x, y) -> (x + 1, y)
    | E -> fun (x, y) -> (x - 1, y)
  in

  let calculate_heat (x, y) ~back_track ~repeat =
    range 1 repeat
    |> Seq.scan (fun acc _ -> back_track acc) (x, y)
    |> Seq.take repeat
    |> List.of_seq
    |> List.fold_left (fun acc (x, y) -> acc + Int2Map.find (x, y) map) 0
  in

  let rec go_internal queue =
    match PQ.pop queue with
    | None -> P.failwith "Reach empty queue but still not reach destination!"
    | Some (((x, y, steps, dir), heat_loss), queue) ->
        if (x, y) = final then
          heat_loss
        else if
          match Hashtbl.get cost (x, y, steps, dir) with
          | None -> false
          | Some (_, visited) -> visited
        then
          go_internal queue
        else
          let () =
            Hashtbl.update cost ~k:(x, y, steps, dir) ~f:(fun _ v ->
                match v with
                | None -> Some (heat_loss, true)
                | Some (l, _) -> Some (l, true))
          in

          next_vector dir
          |> List.map (fun (d, (vx, vy)) ->
                 if d = dir then
                   (d, false, steps + 1, (x + vx, y + vy))
                 else
                   ( d,
                     true,
                     min_step,
                     (x + (vx * min_step), y + (vy * min_step)) ))
          |> List.filter (fun (_, _, steps, (x, y)) ->
                 steps <= max_step && Int2Map.mem (x, y) map)
          |> List.fold_left
               (fun queue (dir, jump, steps, (x, y)) ->
                 let heat =
                   if jump then
                     calculate_heat (x, y) ~back_track:(back_track dir)
                       ~repeat:min_step
                   else
                     Int2Map.find (x, y) map
                 in
                 let (new_heat, visited) =
                   match Hashtbl.get cost (x, y, steps, dir) with
                   | None -> (heat_loss + heat, false)
                   | Some (h, v) -> (min h (heat_loss + heat), v)
                 in
                 let () =
                   Hashtbl.replace cost (x, y, steps, dir) (new_heat, visited)
                 in
                 PQ.push (x, y, steps, dir) new_heat queue)
               queue
          |> go_internal
  in

  [ (0, -min_step, S); (min_step, 0, E) ]
  |> List.map (fun (x, y, dir) ->
         let heat =
           calculate_heat (x, y) ~back_track:(back_track dir) ~repeat:min_step
         in
         ((x, y, min_step, dir), heat))
  |> PQ.of_list
  |> go_internal

let part_one input =
  let grid = parse_grid input ~fvalue:Char.to_digit_exn in
  let final = maxx_miny input in
  go grid final 1 3

let part_two input =
  let grid = parse_grid input ~fvalue:Char.to_digit_exn in
  let final = maxx_miny input in
  go grid final 4 10
