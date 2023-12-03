open Containers
open Share.Func
open Share.Data
open Share.Operator
module P = Share.Printer

let get_neighbors (hx, hy) (tx, ty) =
  move_vectors
  |> List.flat_map (fun (x, y) -> [ (hx + x, hy + y); (tx + x, ty + y) ])
  |> PointSet.of_list

let fold_line acc line nth =
  line
  |> String.to_list
  |> List.mapi (fun i c -> (i, c))
  |> List.group_succ ~eq:(fun (_, a) (_, b) ->
         match (a, b) with
         | ('.', '.') -> true
         | (a, b) -> is_digit a && is_digit b)
  |> List.fold_left
       (fun (n, s) lst ->
         match lst with
         | (_, '.') :: _ -> (n, s)
         | (first, c) :: _ as num when is_digit c ->
             let (last, _) = num |> List.last_opt |> opt_get in
             let number =
               num |> List.map (snd >> String.make 1) |> String.concat "" |> Int.of_string_exn
             in
             (n |> Point2Map.add ((first, nth), (last, nth)) number, s)
         | [ (i, sym) ] -> (n, s |> PointMap.add (i, nth) sym)
         | e -> unreachable P.(lst (tup2 int char)) e)
       acc

let parse_engine input =
  input |> List.foldi (fun acc i line -> fold_line acc line i) (Point2Map.empty, PointMap.empty)

let part_one input =
  let (numbers, symbols) = parse_engine input in
  numbers
  |> Point2Map.filter (fun (head, tail) _ ->
         get_neighbors head tail |> PointSet.exists (fun pos -> PointMap.mem pos symbols))
  |> Point2Map.values
  |> List.of_iter
  |> List.fold_left ( + ) 0

let part_two input =
  let (numbers, symbols) = parse_engine input in
  let gears = symbols |> PointMap.filter (fun _ s -> s = '*') in
  Point2Map.fold
    (fun (head, tail) _num gear_neighbors ->
      get_neighbors head tail
      |> PointSet.filter_map (fun pos ->
             if PointMap.mem pos gears then
               Some pos
             else
               None)
      |> fun gears ->
      PointSet.fold
        (fun gear_pos gn ->
          PointMap.update gear_pos
            (function
              | None -> Some (Point2Set.singleton (head, tail))
              | Some nb -> Some (Point2Set.add (head, tail) nb))
            gn)
        gears gear_neighbors)
    numbers PointMap.empty
  |> PointMap.filter_map (fun _ nb_numbers ->
         if Point2Set.cardinal nb_numbers <> 2 then
           None
         else
           nb_numbers
           |> Point2Set.to_list
           |> List.map (fun pos -> Point2Map.find pos numbers)
           |> List.fold_left ( * ) 1
           |> Option.some)
  |> PointMap.values
  |> List.of_iter
  |> List.fold_left ( + ) 0
