open Containers
open Share.Data
open Share.Func
open Share.Operator
module P = Share.Printer

let to_south = ((0, -1), N)
let to_west = ((-1, 0), E)
let to_east = ((1, 0), W)
let to_north = ((0, 1), S)

let where_next from current_pipe =
  match (from, current_pipe) with
  | (N, '|') | (W, '7') | (E, 'F') -> to_south
  | (N, 'J') | (E, '-') | (S, '7') -> to_west
  | (N, 'L') | (W, '-') | (S, 'F') -> to_east
  | (W, 'J') | (E, 'L') | (S, '|') -> to_north
  | (d, p) -> unreachable P.(pair string char) (show_diection d, p)

let parse input =
  input
  |> List.foldi
       (fun (maze, s) y row ->
         row
         |> String.to_list
         |> List.foldi
              (fun (maze, s) x pipe ->
                ( Int2Map.add (x, -y) pipe maze,
                  if Stdlib.(pipe = 'S') then
                    Some (x, -y)
                  else
                    s ))
              (maze, s))
       (Int2Map.empty, None)
  |> fun (maze, s) -> (maze, (opt_get s, 'S'))

let draw_loop maze ((x, y), _) =
  let rec draw_internal (x, y) from loop =
    match Int2Map.find (x, y) maze with
    | 'S' ->
        let loop = Int2Map.of_list loop in
        loop
        |> Int2Map.update (x, y) (function
             | Some 'S' ->
                 let in_loop = List.for_all (Fun.flip Int2Map.mem loop) in
                 (if in_loop [ (x - 1, y); (x + 1, y) ] then
                    '-'
                  else if in_loop [ (x, y - 1); (x, y + 1) ] then
                    '|'
                  else if in_loop [ (x - 1, y); (x, y - 1) ] then
                    '7'
                  else if in_loop [ (x - 1, y); (x, y + 1) ] then
                    'J'
                  else if in_loop [ (x, y - 1); (x + 1, y) ] then
                    'F'
                  else
                    'L')
                 |> Option.some
             | e -> unreachable P.(option char) e)
    | pipe ->
        let ((vx, vy), from) = where_next from pipe in
        draw_internal (x + vx, y + vy) from (((x, y), pipe) :: loop)
  in

  let (from, next) =
    [
      ([ '|'; 'J'; 'L' ], to_south);
      ([ '-'; 'L'; 'F' ], to_west);
      ([ '-'; 'J'; '7' ], to_east);
      ([ '|'; 'F'; '7' ], to_north);
    ]
    |> List.find_map (fun (next, ((vx, vy), from)) ->
           match Int2Map.get (x + vx, y + vy) maze with
           | None -> None
           | Some pipe when List.mem pipe next -> Some (from, (x + vx, y + vy))
           | _ -> None)
    |> opt_get
  in
  draw_internal next from [ ((x, y), 'S') ]

let part_one input =
  input |> parse ||> draw_loop |> Int2Map.cardinal |> Fun.flip ( / ) 2

let part_two input =
  let intersect_point = function
    | '|' | '-' -> 1
    | 'F' | 'J' -> 0
    | '7' | 'L' -> 1
    | e -> unreachable P.char e
  in

  (* https://en.wikipedia.org/wiki/Point_in_polygon *)
  let rec ray_casting maze loop point count =
    let (x, y) = point in
    if not <| Int2Map.mem point maze then
      count
    else
      (match Int2Map.get point loop with
      | None -> 0
      | Some v -> intersect_point v)
      (* use vector (1, 1) to avoid handling case the ray is on some edges of the loop *)
      |> fun p -> ray_casting maze loop (x + 1, y + 1) (count + p)
  in

  let (maze, s) = input |> parse in
  let loop = draw_loop maze s in
  maze
  |> Int2Map.filter (fun k _ -> not <| Int2Map.mem k loop)
  |> Int2Map.filter (fun k _ ->
         let intersect_count = ray_casting maze loop k 0 in
         intersect_count mod 2 = 1)
  |> Int2Map.cardinal
