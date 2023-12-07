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
  module StringMap = Map.Make (String)
  module CharSet = Set.Make (Char)

  module Int2 = struct
    type t = int * int

    let compare = Stdlib.compare
  end

  module Int3 = struct
    type t = int * int * int

    let compare = Stdlib.compare
  end

  module IntSet = Set.Make (Int)
  module IntMap = Map.Make (Int)
  module Int2Set = Set.Make (Int2)
  module Int2Map = Map.Make (Int2)
  module Int3Set = Set.Make (Int3)
  module Int3Map = Map.Make (Int3)

  let move_vectors =
    [ (-1, 1); (0, 1); (1, 1); (-1, 0); (1, 0); (-1, -1); (0, -1); (1, -1) ]
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

  let range first last =
    Seq.iterate (( + ) 1) first |> Seq.take (last - first + 1)

  let rec compare_list ?(cmp = Stdlib.compare) l1 l2 =
    match (l1, l2) with
    | ([], []) -> 0
    | ([], _) -> -1
    | (_, []) -> 1
    | (h :: t, hh :: tt) -> (
        match cmp h hh with
        | 0 -> compare_list ~cmp t tt
        | c -> c)

  let rec gcd a b =
    if a = b then
      a
    else
      gcd (min a b) (a |> ( - ) b |> abs)

  let lcm a b = a * b / gcd a b
end
