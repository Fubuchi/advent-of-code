open Share

let part_one instructions =
  instructions
  |> String.to_seq
  |> Seq.fold_left
       (fun floor ins ->
         match ins with
         | '(' -> floor + 1
         | ')' -> floor - 1
         | c -> unreachable Fmt.char c)
       0

let part_two instructions =
  instructions
  |> String.to_seqi
  |> Seq.fold_left
       (fun (floor, index) (i, ins) ->
         if floor = -1 then
           (floor, index)
         else
           match ins with
           | '(' -> (floor + 1, i)
           | ')' -> (floor - 1, i)
           | c -> unreachable Fmt.char c)
       (0, 0)
  |> snd
  |> ( + ) 1
