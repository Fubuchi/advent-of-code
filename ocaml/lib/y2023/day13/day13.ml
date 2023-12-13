open Containers
open Share.Data
open Share.Func

let slice lst = lst |> windowed 2 1 ~discard:true |> List.map pair_of_list

let check_mirror map (a1, a2) lst ~max_diff ~exit ~to_point ~next =
  let rec check_internal (a1, a2) diff_count =
    if exit a1 a2 then
      diff_count = max_diff
    else
      let diff =
        lst
        |> List.count (fun b ->
               let (p1, p2) = to_point a1 a2 b in
               let a = Int2Map.find p1 map in
               let b = Int2Map.find p2 map in
               Stdlib.(a <> b))
      in
      let new_diff = diff_count + diff in
      if new_diff > max_diff then
        false
      else
        check_internal (next a1 a2) new_diff
  in
  check_internal (a1, a2) 0

let solve_pattern pattern ~max_diff =
  let map = parse_grid pattern in
  let (max_x, min_y) = maxx_miny pattern in
  let lst_x = range 0 max_x |> Seq.to_list in
  let lst_y = range 0 ~step:(-1) min_y |> Seq.to_list in

  lst_x
  |> slice
  |> List.find_opt (fun x ->
         check_mirror map x lst_y ~max_diff
           ~exit:(fun x1 x2 -> x1 < 0 || x2 > max_x)
           ~to_point:(fun x1 x2 y -> ((x1, y), (x2, y)))
           ~next:(fun x1 x2 -> (x1 - 1, x2 + 1)))
  |> function
  | Some (x1, _) -> x1 + 1
  | _ -> (
      lst_y
      |> slice
      |> List.find_opt (fun y ->
             check_mirror map y lst_x ~max_diff
               ~exit:(fun y1 y2 -> y1 > 0 || y2 < min_y)
               ~to_point:(fun y1 y2 x -> ((x, y1), (x, y2)))
               ~next:(fun y1 y2 -> (y1 + 1, y2 - 1)))
      |> function
      | Some (y1, _) -> (abs y1 + 1) * 100
      | _ -> Fmt.failwith "Can not find mirror")

let solve input ~max_diff =
  input
  |> List.group_succ ~eq:(fun a b -> Stdlib.(a <> "" && b <> ""))
  |> List.filter (function
       | [ "" ] -> false
       | _ -> true)
  |> List.fold_left (fun acc line -> acc + solve_pattern line ~max_diff) 0

let part_one = solve ~max_diff:0
let part_two = solve ~max_diff:1
