open Share.Func
open Share.Data

type direction =
  | North
  | East
  | South
  | West

let parse_type = function
  | '^' -> North
  | '>' -> East
  | 'v' -> South
  | '<' -> West
  | e -> unreachable Fmt.char e

let parse_input input = input |> String.to_seq |> Seq.map parse_type

let move (x, y) = function
  | North -> (x, y + 1)
  | East -> (x + 1, y)
  | South -> (x, y - 1)
  | West -> (x - 1, y)

let update_houses (houses, pos) dir =
  let next = move pos dir in
  if Int2Set.mem next houses then
    (houses, next)
  else
    (Int2Set.add next houses, next)

let part_one input =
  input
  |> parse_input
  |> Seq.fold_left update_houses (Int2Set.singleton (0, 0), (0, 0))
  |> fst
  |> Int2Set.cardinal

let part_two input =
  input
  |> parse_input
  |> Seq.fold_lefti
       (fun (houses, santa_pos, robo_pos) turn dir ->
         if turn mod 2 = 0 then
           let (houses, santa_pos) = update_houses (houses, santa_pos) dir in
           (houses, santa_pos, robo_pos)
         else
           let (houses, robo_pos) = update_houses (houses, robo_pos) dir in
           (houses, santa_pos, robo_pos))
       (Int2Set.singleton (0, 0), (0, 0), (0, 0))
  |> (fun (houses, _, _) -> houses)
  |> Int2Set.cardinal
