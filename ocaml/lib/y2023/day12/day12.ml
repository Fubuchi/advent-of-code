open Containers
open Share.Func
module P = Share.Printer

let parse input =
  input
  |> List.map (fun line ->
         match String.split ~by:" " line with
         | [ record; damage ] ->
             ( String.to_list record,
               damage |> String.split ~by:"," |> List.map Int.of_string_exn )
         | e -> unreachable Fmt.(Dump.list string) e)

let rebuild_record self = function
  | ([], [], 0) -> 1
  | ([], _, 0) -> 0
  | ([], [], _) -> 0
  | (c :: r, d, filled) -> (
      match c with
      | '#' -> self (r, d, filled + 1)
      | '.' ->
          if filled > 0 then
            match d with
            | d :: d_rest when d = filled -> self (r, d_rest, 0)
            | _ -> 0
          else
            self (r, d, 0)
      | '?' -> self ('#' :: r, d, filled) + self ('.' :: r, d, filled)
      | e -> unreachable Fmt.char e)
  | e -> unreachable P.(triple (lst char) (lst int) int) e

let rebuild_record = memo_rec rebuild_record

let part_one input =
  input
  |> parse
  |> List.map (fun (r, d) -> (r @ [ '.' ], d))
  |> List.fold_left (fun acc (r, d) -> acc + rebuild_record (r, d, 0)) 0

let part_two input =
  input
  |> List.map (fun line ->
         line
         |> String.split_on_char ' '
         |> function
         | [ record; damage ] ->
             let record =
               Seq.repeat record |> Seq.take 5 |> String.concat_seq ~sep:"?"
             in
             let damage =
               Seq.repeat damage |> Seq.take 5 |> String.concat_seq ~sep:","
             in
             record ^ " " ^ damage
         | e -> unreachable Fmt.(Dump.list string) e)
  |> part_one
