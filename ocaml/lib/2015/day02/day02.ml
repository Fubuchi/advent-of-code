open Share

let parse input =
  input
  |> List.map (String.split_on_char 'x')
  |> List.map (fun present_sizes -> present_sizes |> List.map int_of_string)

let part_one sizes =
  sizes
  |> parse
  |> List.map (fun present_sizes ->
         present_sizes
         |> combnk 2
         |> List.map (function
              | [ x; y ] -> x * y
              | u -> unreachable u)
         |> List.sort compare
         |> function
         | smallest :: _ as areas ->
             smallest + (areas |> List.fold_left ( + ) 0 |> ( * ) 2)
         | u -> unreachable u)
  |> List.fold_left ( + ) 0

let part_two sizes =
  sizes
  |> parse
  |> List.map (List.sort compare)
  |> List.map (function
       | [ x; y; z ] -> (2 * (x + y)) + (x * y * z)
       | u -> unreachable u)
  |> List.fold_left ( + ) 0
