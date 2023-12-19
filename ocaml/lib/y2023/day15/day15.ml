open Containers
open Share.Data
open Share.Func
open Share.Ext
module P = Share.Printer

type op =
  | Assign of int
  | Remove

let hash str =
  str
  |> String.to_list
  |> List.fold_left
       (fun h c ->
         c |> Char.code |> ( + ) h |> ( * ) 17 |> Fun.flip ( mod ) 256)
       0

let part_one input =
  input |> String.split ~by:"," |> List.map hash |> List.fold_left ( + ) 0

let part_two input =
  let re = Pcre.regexp "(\\w+)(=|-)(\\d{0,})" in
  let boxes =
    input
    |> String.split ~by:","
    |> List.map (fun line ->
           Pcre.extract ~rex:re line
           |> function
           | [| _; label; "="; value |] ->
               (label, Assign (Int.of_string_exn value))
           | [| _; label; "-"; "" |] -> (label, Remove)
           | e -> unreachable P.(array string) e)
    |> List.fold_left
         (fun boxes op ->
           match op with
           | (label, Remove) ->
               let index = hash label in
               boxes
               |> IntMap.update index (function
                    | None -> None
                    | Some lens ->
                        lens
                        |> List.remove_if (fun (lb, _) -> Stdlib.(lb = label))
                        |> Option.some)
           | (label, Assign value) ->
               let index = hash label in
               boxes
               |> IntMap.update index (function
                    | None -> Some [ (label, value) ]
                    | Some lens ->
                        let test (lb, _) = Stdlib.(lb = label) in
                        (if List.exists test lens then
                           lens
                           |> List.update_if ~eq:test ~map:(fun (lb, _) ->
                                  (lb, value))
                         else
                           (label, value) :: lens)
                        |> Option.some))
         IntMap.empty
  in

  range 0 255
  |> Seq.fold_left
       (fun acc box ->
         match IntMap.get box boxes with
         | None -> acc
         | Some lens ->
             lens
             |> List.rev
             |> List.mapi (fun slot (_, v) -> (box + 1) * (slot + 1) * v)
             |> List.fold_left ( + ) 0
             |> ( + ) acc)
       0
