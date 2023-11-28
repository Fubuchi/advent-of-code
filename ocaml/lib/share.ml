module StringMap = BatMap.Make (String)
module CharSet = BatSet.Make (Char)

module Point = struct
  type t = int * int

  let compare = compare
end

module PointSet = Set.Make (Point)

let ( << ) f g x = f (g x)
let ( >> ) f g x = g (f x)
let ( <| ) f x = f x
let ( ||> ) (x, y) f = f x y

let tap f x =
  let () = f x in
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
        let take = source |> BatList.take size in
        let left_over = source |> BatList.drop step in
        if List.length left_over < size && discard then
          take :: result
        else
          windowed_internal left_over (take :: result)
  in

  windowed_internal lst [] |> List.rev
