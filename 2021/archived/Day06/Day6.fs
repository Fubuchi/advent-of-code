module Day6

open System.IO

let init days =
    let fishes = [| for _ in 0..8 do 0UL |]
    let addFish fishes fish =
        fishes
        |> Array.mapi (fun i f -> if i = fish then f + 1UL else f)

    days
    |> Array.fold addFish fishes

let nextDay (fishes: uint64 array) =
    let additionalSix, nextEight = fishes.[0], fishes.[0]

    Array.append fishes [| nextEight |]
    |> Array.pairwise
    |> Array.mapi (fun i (_, next) ->
        if i <> 6 then
            next
        else
            next + additionalSix)

let solve left fishes =
    let rec loop left fishes =
        if left = 0 then
            fishes
        else
            fishes |> nextDay |> loop (left - 1)

    loop left fishes |> Array.sum

let input =
    (File.ReadAllLines "input.txt" |> Array.head)
        .Split [| ',' |]
    |> Array.map int
    |> init

solve 80 input |> printfn "First part: %d"
solve 256 input |> printfn "Second part: %d"
