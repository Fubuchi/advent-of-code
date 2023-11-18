module StringMap = BatMap.Make (String)

let ( << ) f g x = f (g x)
let ( >> ) f g x = g (f x)

let unreachable a =
  BatPervasives.dump a
  |> Format.sprintf "Branch value is unreachable: %s"
  |> failwith

let rec combnk k lst =
  if k = 0 then
    [ [] ]
  else
    let rec inner = function
      | [] -> []
      | x :: xs -> List.map (fun z -> x :: z) (combnk (k - 1) xs) :: inner xs
    in
    List.concat (inner lst)
