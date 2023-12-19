open Containers

module Printer = struct
  include Fmt

  let arr t = Fmt.Dump.array t
  let lst t = Fmt.Dump.list t
  let pair l r = Fmt.pair l r ~sep:Fmt.comma

  let triple pp_fst pp_snd pp_third =
    let fst (a, _, _) = a in
    let snd (_, b, _) = b in
    let thrd (_, _, c) = c in
    parens
      (using fst (box pp_fst)
      ++ comma
      ++ using snd (box pp_snd)
      ++ comma
      ++ using thrd (box pp_third))
end

module Data = struct
  type diection =
    | N
    | W
    | E
    | S
  [@@deriving show]

  module StringMap = Map.Make (String)
  module CharSet = Set.Make (Char)

  module Int2 = struct
    type t = int * int [@@deriving show]

    let compare = Stdlib.compare
  end

  module Int3 = struct
    type t = int * int * int

    let compare = Stdlib.compare
  end

  module Int4 = struct
    type t = int * int * int * int

    let compare = Stdlib.compare
  end

  module IntSet = Set.Make (Int)
  module IntMap = Map.Make (Int)
  module Int2Set = Set.Make (Int2)
  module Int2Map = Map.Make (Int2)
  module Int3Set = Set.Make (Int3)
  module Int3Map = Map.Make (Int3)
  module Int4Map = Map.Make (Int4)

  let move_vectors_8 =
    [ (-1, 1); (0, 1); (1, 1); (-1, 0); (1, 0); (-1, -1); (0, -1); (1, -1) ]

  let move_vector_4 = [ (-1, 0); (0, 1); (1, 0); (0, -1) ]
end

module Operator = struct
  let ( << ) f g x = f (g x)
  let ( >> ) f g x = g (f x)
  let ( <| ) f x = f x
  let ( ||> ) (x, y) f = f x y
  let ( |||> ) (x, y, z) f = f x y z
  let ( = ) = Stdlib.( = )
end

module Func = struct
  open Data

  let memo_rec f =
    let h = Hashtbl.create 16 in
    let rec g x =
      try Hashtbl.find h x
      with Not_found ->
        let y = f g x in
        Hashtbl.add h x y;
        y
    in
    g

  let tap f x =
    f x;
    x

  let opt_get = function
    | Some v -> v
    | _ -> failwith "Empty value!"

  let is_digit = function
    | '0' .. '9' -> true
    | _ -> false

  let pair_of_list = function
    | [ x; y ] -> (x, y)
    | e -> Fmt.failwith "Invalid list lenght %d" (List.length e)

  let parse_grid ~fvalue input =
    input
    |> List.foldi
         (fun map y line ->
           line
           |> String.to_list
           |> List.foldi (fun m x v -> Int2Map.add (x, -y) (fvalue v) m) map)
         Int2Map.empty

  let maxx_miny input =
    let max_x = input |> List.hd |> String.length |> Fun.flip ( - ) 1 in
    let min_y = input |> List.length |> ( - ) 1 in
    (max_x, min_y)

  let unreachable printer a =
    Fmt.failwith "Branch value is unreachable: %a" printer a

  let rec combnk k lst =
    if k = 0 then
      [ [] ]
    else
      let rec inner = function
        | [] -> []
        | x :: xs -> List.map (fun z -> x :: z) (combnk (k - 1) xs) :: inner xs
      in
      List.concat (inner lst)

  let windowed size step ?(discard = false) lst =
    let rec windowed_internal source result =
      match source with
      | [] -> result
      | _ ->
          let take = source |> List.take size in
          let left_over = source |> List.drop step in
          if List.length left_over < size && discard then
            take :: result
          else
            windowed_internal left_over (take :: result)
    in

    windowed_internal lst [] |> List.rev

  let range first last ?(step = 1) =
    Seq.iterate (( + ) step) first |> Seq.take (abs (last - first) + 1)

  let rec compare_list ?(cmp = Stdlib.compare) l1 l2 =
    match (l1, l2) with
    | ([], []) -> 0
    | ([], _) -> -1
    | (_, []) -> 1
    | (h :: t, hh :: tt) -> (
        match cmp h hh with
        | 0 -> compare_list ~cmp t tt
        | c -> c)

  let move_to_dir (x, y) = function
    | N -> (x, y + 1)
    | S -> (x, y - 1)
    | E -> (x + 1, y)
    | W -> (x - 1, y)

  let rec gcd a b =
    if a = b then
      a
    else
      gcd (min a b) (a |> ( - ) b |> abs)

  let lcm a b = a * b / gcd a b
end

module Ext = struct
  module List = struct
    include List

    let remove_if f lst =
      let rec remove_if' aux = function
        | [] -> List.rev aux
        | x :: xs ->
            if f x then
              remove_if' aux xs
            else
              remove_if' (x :: aux) xs
      in
      remove_if' [] lst

    let update_if ~eq ~map lst =
      let rec update_if' aux = function
        | [] -> List.rev aux
        | x :: xs ->
            if eq x then
              update_if' (map x :: aux) xs
            else
              update_if' (x :: aux) xs
      in
      update_if' [] lst
  end

  module Char = struct
    include Char

    let to_digit_exn = function
      | c when Func.is_digit c -> code c - code '0'
      | c -> Printer.failwith "Invalid value: %c" c
  end
end
