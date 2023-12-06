open Containers
open Share.Func
open Share.Data
open Share.Operator
module P = Share.Printer

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
               num
               |> List.map (snd >> String.make 1)
               |> String.concat ""
               |> Int.of_string_exn
             in
             (n |> Int3Map.add (first, last, nth) number, s)
         | [ (i, sym) ] -> (n, s |> Int2Map.add (i, nth) (sym, Int3Set.empty))
         | e -> unreachable P.(lst (pair int char)) e)
       acc

let parse input =
  input
  |> List.foldi
       (fun acc i line -> fold_line acc line i)
       (Int3Map.empty, Int2Map.empty)

let nearby_of_number (fst, lst, row) =
  move_vectors
  |> List.flat_map (fun (vx, vy) ->
         range fst lst
         |> Seq.map (fun x -> (x + vx, row + vy))
         |> Seq.uniq Stdlib.( = )
         |> Seq.to_list)

let nearby_number_of_symbol numbers symbols =
  numbers
  |> Int3Map.keys
  |> List.of_iter
  |> List.map (fun num -> (num, nearby_of_number num))
  |> List.fold_left
       (fun symbols (pos_num, nearby_pos) ->
         match
           List.find_opt (fun pos -> Int2Map.mem pos symbols) nearby_pos
         with
         | None -> symbols
         | Some pos ->
             symbols
             |> Int2Map.update pos (function
                  | None -> None
                  | Some (sym, nearby_num) ->
                      Some (sym, nearby_num |> Int3Set.add pos_num)))
       symbols

let part_one input =
  let (numbers, symbols) = parse input in
  numbers
  |> Int3Map.filter (fun pos _ ->
         nearby_of_number pos
         |> List.exists (fun pos -> Int2Map.mem pos symbols))
  |> Int3Map.values
  |> Iter.fold ( + ) 0

let part_two input =
  let (numbers, symbols) = parse input in
  (numbers, symbols)
  ||> nearby_number_of_symbol
  |> Int2Map.values
  |> Iter.filter_map (function
       | (c, _) when Stdlib.(c <> '*') -> None
       | (_, nearby_num) when Int3Set.cardinal nearby_num |> ( <> ) 2 -> None
       | (_, nearby_num) ->
           nearby_num
           |> Int3Set.to_list
           |> (function
                | [ a; b ] -> Int3Map.find a numbers * Int3Map.find b numbers
                | e -> unreachable P.(lst (triple int int int)) e)
           |> Option.some)
  |> Iter.fold ( + ) 0
