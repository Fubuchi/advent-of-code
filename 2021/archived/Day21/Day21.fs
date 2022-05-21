module Day21

let landing x = (x - 1) % 10 + 1

let roll d = (d - 1) % 100 + 1

let rec practice score1 score2 pos1 pos2 dice turn =
    let pos1 =
        landing (
            pos1
            + roll dice
            + roll (dice + 1)
            + roll (dice + 2)
        )

    let pos2 =
        landing (
            pos2
            + roll (dice + 3)
            + roll (dice + 4)
            + roll (dice + 5)
        )

    match score1 + pos1 >= 1000, score2 + pos2 >= 1000 with
    | true, _ -> score2 * (turn + 3)
    | _, true -> (score1 + pos1) * (turn + 6)
    | _ -> practice (score1 + pos1) (score2 + pos2) pos1 pos2 (roll (dice + 6)) (turn + 6)

let enterUniversal score1 score2 pos1 pos2 =

    let universals =
        [ for x in 1 .. 3 do
              for y in 1 .. 3 do
                  for z in 1 .. 3 -> x + y + z ]
        |> List.countBy id
        |> Map

    let range = [ 3 .. 9 ]

    let rec loop score1 score2 pos1 pos2 p1Turn unis =
        if score1 >= 21 then
            (unis, 0L)
        else if score2 >= 21 then
            (0L, unis)
        else
            range
            |> Seq.fold
                (fun (wins1, wins2) u ->
                    let us = Map.find u universals |> int64

                    let w1, w2 =
                        if p1Turn then
                            let pos1 = landing (pos1 + u)
                            let score1 = score1 + pos1
                            loop score1 score2 pos1 pos2 false (unis * us)
                        else
                            let pos2 = landing (pos2 + u)
                            let score2 = score2 + pos2
                            loop score1 score2 pos1 pos2 true (unis * us)

                    (wins1 + w1, wins2 + w2))
                (0L, 0L)

    loop score1 score2 pos1 pos2 true 1L

practice 0 0 5 9 1 0 |> printfn "Frist part: %d"

enterUniversal 0 0 5 9
||> max
|> printfn "Second part: %d"
