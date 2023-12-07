open Containers
open Share.Func
module P = Share.Printer

let card_rank ~joker = function
  | 'A' -> 14
  | 'K' -> 13
  | 'Q' -> 12
  | 'J' ->
      if joker then
        1
      else
        11
  | 'T' -> 10
  | o -> o |> Char.to_string |> Int.of_string_exn

let hand_rank = function
  | [ [ _; _; _; _; _ ] ] -> 7
  | [ _; [ _; _; _; _ ] ] -> 6
  | [ [ _; _ ]; [ _; _; _ ] ] -> 5
  | [ _; _; [ _; _; _ ] ] -> 4
  | [ _; [ _; _ ]; [ _; _ ] ] -> 3
  | [ _; _; _; [ _; _ ] ] -> 2
  | _ -> 1

let hand_rank_promoted = function
  | [ [ 'J' ]; _; _; _; _ ] -> 2
  | [ [ 'J' ]; _; _; [ _; _ ] ] -> 4
  | [ [ 'J' ]; [ _; _ ]; [ _; _ ] ] -> 5
  | [ [ 'J' ]; _; [ _; _; _ ] ] -> 6
  | [ [ 'J' ]; [ _; _; _; _ ] ] -> 7
  | [ _; _; _; [ 'J'; _ ] ] -> 4
  | [ _; [ 'J'; _ ]; [ _; _ ] ] -> 6
  | [ [ 'J'; _ ]; [ _; _; _ ] ] -> 7
  | [ _; _; [ 'J'; _; _ ] ] -> 6
  | [ [ _; _ ]; [ 'J'; _; _ ] ] -> 7
  | [ _; [ 'J'; _; _; _ ] ] -> 7
  | no_joke -> hand_rank no_joke

let compare_card ~joker c1 c2 =
  compare (card_rank ~joker c1) (card_rank ~joker c2)

let compare_hand ~joker (st1, h1) (st2, h2) =
  match compare st1 st2 with
  | 0 -> compare_list ~cmp:(compare_card ~joker) h1 h2
  | c -> c

let parse_hand ~joker hand_rank hand =
  let hand = hand |> String.to_list in
  let card_group =
    hand
    |> List.group_by ~eq:Stdlib.( = )
    |> List.sort (fun l1 l2 ->
           match compare (List.length l1) (List.length l2) with
           | 0 ->
               let h1 = List.hd l1 in
               let h2 = List.hd l2 in
               compare (card_rank ~joker h1) (card_rank ~joker h2)
           | c -> c)
  in
  (hand_rank card_group, hand)

let parse ~joker hand_rank input =
  input
  |> List.map (String.split_on_char ' ')
  |> List.map (function
       | [ hand; bid ] ->
           (parse_hand ~joker hand_rank hand, Int.of_string_exn bid)
       | e -> unreachable P.(lst string) e)

let solve hand_rank ~joker input =
  input
  |> parse ~joker hand_rank
  |> List.sort (fun (h1, _) (h2, _) -> compare_hand ~joker h1 h2)
  |> List.fold_left
       (fun (sum, rank) (_, bid) -> ((rank * bid) + sum, rank + 1))
       (0, 1)
  |> fst

let part_one = solve hand_rank ~joker:false
let part_two = solve hand_rank_promoted ~joker:true
