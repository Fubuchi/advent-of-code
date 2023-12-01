open Containers

module Printer = struct
  let string = Fmt.string
  let arr t = Fmt.(Dump.array t)
  let lst t = Fmt.(Dump.list t)
end

module Data = struct
  module StringMap = Map.Make (String)
  module CharSet = Set.Make (Char)

  module Point = struct
    type t = int * int

    let compare = Stdlib.compare
  end

  module PointSet = Set.Make (Point)
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
end
