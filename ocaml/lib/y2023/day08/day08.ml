open Containers
open Share.Func
open Share.Data
open Share.Operator
module P = Share.Printer

let parse = function
  | steps :: "" :: map ->
      ( steps |> Seq.of_string |> Seq.cycle,
        map
        |> List.map (fun line ->
               line
               |> String.replace ~sub:"(" ~by:""
               |> String.replace ~sub:")" ~by:""
               |> String.split ~by:" = "
               |> pair_of_list
               |> fun (destination, option) ->
               (destination, option |> String.split ~by:", " |> pair_of_list))
        |> StringMap.of_list )
  | e -> unreachable P.(lst string) e

let pick = function
  | 'L' -> fst
  | 'R' -> snd
  | e -> unreachable P.char e

let go ~go_when start steps map =
  steps
  |> Seq.scan
       (fun curr step ->
         let option = StringMap.find curr map in
         (pick step) option)
       start
  |> Seq.take_while go_when
  |> Seq.length

let part_one input =
  input |> parse ||> go "AAA" ~go_when:(fun des -> Stdlib.(des <> "ZZZ"))

let part_two input =
  let (steps, map) = parse input in
  let starts =
    map |> StringMap.keys |> Iter.filter (String.ends_with ~suffix:"Z")
  in
  let first_step = steps |> Seq.head_exn in
  let steps = steps |> Seq.drop 1 in
  starts
  |> Iter.map (fun start ->
         let option = StringMap.find start map in
         let next = (pick first_step) option in
         go next steps map ~go_when:(fun des ->
             not <| String.ends_with ~suffix:"Z" des)
         |> ( + ) 1)
  |> Iter.to_list
  |> List.reduce lcm
  |> Option.get_exn_or "boo!"
