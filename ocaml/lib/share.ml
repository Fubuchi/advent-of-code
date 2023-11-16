module StringMap = BatMap.Make (String)

let ( << ) f g x = f (g x)
let ( >> ) f g x = g (f x)

let unreachable a =
  BatPervasives.dump a
  |> Format.sprintf "Branch value is unreachable: %s"
  |> failwith
