open Containers
open Share.Func
open Share.Operator
module P = Share.Printer

type game = {
  id : int;
  red : int option;
  blue : int option;
  green : int option;
}

let parse_game input =
  let update_dice new_val = function
    | Some old -> Some (max old new_val)
    | _ -> Some new_val
  in
  input
  |> List.mapi (fun i raw ->
         raw
         |> (Pcre.replace_first ~pat:"Game \\d+: " ~templ:""
            >> Pcre.split ~pat:"; |, ")
         |> List.map (String.split ~by:" ")
         |> List.map (function
              | [ count; dice ] -> (dice, Int.of_string_exn count)
              | e -> unreachable P.(lst string) e)
         |> List.fold_left
              (fun game pull ->
                match pull with
                | ("red", count) ->
                    { game with red = update_dice count game.red }
                | ("blue", count) ->
                    { game with blue = update_dice count game.blue }
                | ("green", count) ->
                    { game with green = update_dice count game.green }
                | e -> unreachable P.(pair string int) e)
              { id = i + 1; red = None; blue = None; green = None })

let part_one input =
  let validate value ~limit input =
    match value input with
    | Some v ->
        if v <= limit then
          Some input
        else
          None
    | _ -> Some input
  in
  input
  |> parse_game
  |> List.filter_map (fun game ->
         Option.(
           game
           |> some
           >>= validate (fun g -> g.red) ~limit:12
           >>= validate (fun g -> g.green) ~limit:13
           >>= validate (fun g -> g.blue) ~limit:14
           |> map (fun x -> x.id)))
  |> List.fold_left ( + ) 0

let part_two input =
  input
  |> parse_game
  |> List.fold_left
       (fun total game ->
         let (r, b, g) =
           (opt_get game.red, opt_get game.blue, opt_get game.green)
         in
         total + (r * b * g))
       0
