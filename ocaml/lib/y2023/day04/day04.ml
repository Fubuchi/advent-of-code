open Containers
open Share.Data
open Share.Func
module P = Share.Printer

let card_re = Pcre.regexp "Card\\s+\\d+:\\s+"
let mid_re = Pcre.regexp "\\s+\\|\\s+"
let num_re = Pcre.regexp "\\s+"

let parse_line line =
  line
  |> Pcre.replace_first ~rex:card_re ~templ:""
  |> Pcre.split ~rex:mid_re
  |> function
  | [ winning; numbers ] ->
      ( Pcre.split ~rex:num_re winning
        |> List.map Int.of_string_exn
        |> IntSet.of_list,
        Pcre.split ~rex:num_re numbers
        |> List.map Int.of_string_exn
        |> IntSet.of_list )
  | e -> unreachable P.(lst string) e

let parse input =
  input
  |> List.mapi (fun i line ->
         let (winning, numbers) = parse_line line in
         (i, (winning, numbers)))

let win_count winning numbers = IntSet.inter winning numbers |> IntSet.cardinal

let part_one input =
  input
  |> parse
  |> List.map (fun (_, (winning, numbers)) ->
         match win_count winning numbers with
         | 0 -> 0
         | c -> Int.pow 2 (c - 1))
  |> List.fold_left ( + ) 0

let part_two input =
  let games = input |> parse in
  let starter = games |> List.map (fun (k, _) -> (k, 1)) |> IntMap.of_list in
  games
  |> List.fold_left
       (fun games (i, (winning, numbers)) ->
         match win_count winning numbers with
         | 0 -> games
         | win_num ->
             let card = IntMap.find i games in
             range (i + 1) (i + win_num)
             |> Seq.fold_left
                  (fun g i ->
                    g
                    |> IntMap.update i (function
                         | None -> None
                         | Some c -> Some (c + card)))
                  games)
       starter
  |> IntMap.values
  |> Iter.fold ( + ) 0
