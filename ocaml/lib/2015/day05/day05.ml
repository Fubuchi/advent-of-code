open Share

let vowels = [ 'a'; 'e'; 'i'; 'o'; 'u' ]
let bad_pairs = [ [ 'a'; 'b' ]; [ 'c'; 'd' ]; [ 'p'; 'q' ]; [ 'x'; 'y' ] ]

let part_one input =
  input
  |> List.map (String.to_seq >> List.of_seq >> windowed 2 1)
  |> BatList.count_matching (fun line ->
         let three_vowels =
           line
           |> BatList.count_matching (function
                | c :: _ -> List.mem c vowels
                | _ -> false)
           |> Fun.flip ( >= ) 3
         in
         let has_double =
           line
           |> List.exists (function
                | [ c1; c2 ] -> c1 = c2
                | _ -> false)
         in
         let do_not_have_bad_pairs =
           line
           |> List.for_all (function
                | [ _; _ ] as pair -> bad_pairs |> List.mem pair |> not
                | _ -> true)
         in
         three_vowels && has_double && do_not_have_bad_pairs)

let part_two input =
  let rec double_pair lst =
    match lst with
    | pair :: pair_overlap :: tail ->
        List.mem pair tail || double_pair (pair_overlap :: tail)
    | _ -> false
  in

  input
  |> List.map (String.to_seq >> List.of_seq)
  |> BatList.count_matching (fun line ->
         let double_pair = line |> windowed 2 1 ~discard:true |> double_pair in
         let uwu =
           line
           |> windowed 3 1 ~discard:true
           |> List.exists (function
                | [ c1; _; c2 ] -> c1 = c2
                | _ -> false)
         in
         double_pair && uwu)
