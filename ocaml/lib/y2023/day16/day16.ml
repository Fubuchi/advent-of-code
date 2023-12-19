open Containers
open Share.Data
open Share.Func
open Share.Operator
module P = Share.Printer

let parse_grid = parse_grid ~fvalue:Fun.id
let move (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

let vector = function
  | N -> (0, 1)
  | E -> (1, 0)
  | S -> (0, -1)
  | W -> (-1, 0)

let relfect = function
  | (S, '/') -> [ W ]
  | (S, '\\') -> [ E ]
  | (S, '|') -> [ S ]
  | (S, '-') -> [ W; E ]
  | (E, '/') -> [ N ]
  | (E, '\\') -> [ S ]
  | (E, '|') -> [ S; N ]
  | (E, '-') -> [ E ]
  | (N, '/') -> [ E ]
  | (N, '\\') -> [ W ]
  | (N, '|') -> [ N ]
  | (N, '-') -> [ E; W ]
  | (W, '/') -> [ S ]
  | (W, '\\') -> [ N ]
  | (W, '|') -> [ S; N ]
  | (W, '-') -> [ W ]
  | (d, '.') -> [ d ]
  | (d, m) -> unreachable P.(pair string char) (show_diection d, m)

let cast map rays =
  let visited = Hashtbl.create 16 in
  let rec cast' rays =
    match rays with
    | [] -> ()
    | (dir, current) :: xs -> (
        if Hashtbl.mem visited (dir, current) || not <| Int2Map.mem current map
        then
          cast' xs
        else
          let () = Hashtbl.add visited (dir, current) current in
          let mirror = Int2Map.find current map in
          (dir, mirror)
          |> relfect
          |> List.map (fun dir -> (dir, dir |> vector |> move current))
          |> function
          | [ n ] -> cast' (n :: xs)
          | [ n1; n2 ] -> cast' (n1 :: n2 :: xs)
          | e ->
              e
              |> List.map (fun (d, c) -> (show_diection d, c))
              |> unreachable P.(lst (pair string (pair int int))))
  in

  let () = cast' rays in
  visited |> Hashtbl.values |> Int2Set.of_iter |> Int2Set.cardinal

let part_one input =
  let map = parse_grid input in
  cast map [ (E, (0, 0)) ]

let part_two input =
  let map = parse_grid input in
  let (max_x, min_y) = maxx_miny input in
  let starts =
    (List.range 0 max_x
    |> List.flat_map (fun x -> [ (S, (x, 0)); (N, (x, min_y)) ]))
    @ (List.range 0 min_y
      |> List.flat_map (fun y -> [ (E, (0, y)); (W, (max_x, y)) ]))
  in
  starts
  |> List.map (fun start -> cast map [ start ])
  |> List.sort (fun a b -> b - a)
  |> List.hd
