open Containers

module Printer = struct
  let string = Fmt.string
  let int = Fmt.int
  let char = Fmt.char
  let arr t = Fmt.(Dump.array t)
  let lst t = Fmt.(Dump.list t)
  let tup2 l r = Fmt.(pair l r)
end

module Data = struct
  module StringMap = Map.Make (String)
  module CharSet = Set.Make (Char)

  module Point = struct
    type t = int * int

    let compare = Stdlib.compare
  end

  module Point2 = struct
    type t = (int * int) * (int * int)

    let compare = Stdlib.compare
  end

  module PointSet = Set.Make (Point)
  module PointMap = Map.Make (Point)
  module Point2Set = Set.Make (Point2)
  module Point2Map = Map.Make (Point2)

  let move_vectors = [ (-1, 1); (0, 1); (1, 1); (-1, 0); (1, 0); (-1, -1); (0, -1); (1, -1) ]
end

module Operator = struct
  let ( << ) f g x = f (g x)
  let ( >> ) f g x = g (f x)
  let ( <| ) f x = f x
  let ( ||> ) (x, y) f = f x y
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

  let unreachable printer a = Fmt.failwith "Branch value is unreachable: %a" printer a

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
end
