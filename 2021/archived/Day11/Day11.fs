module Day11

open System.IO

let (|EmptySet|_|) a = if Set.isEmpty a then Some () else None

let (<&>) f g x = f x && g x

let (<.>) f x y = f y x

let locate (o: 'a [,]) (x, y) = o.[y, x]

let flashed = snd
let energy = fst

let X = 9
let Y = 9

let inBound (x, y) = not <| (x < 0 || x > X || y < 0 || y > Y)

let neighbors (x, y) =
    [| (0, -1); (0, 1); (1, 0); (-1, 0); (-1, -1); (-1, 1); (1, -1); (1, 1) |]
    |> Array.map (fun (vx, vy) -> x + vx, y + vy)
    |> Array.filter inBound

let flash ocean =
    let waitingToFlash ocean =
        [| for x in 0 .. X do
               for y in 0 .. Y -> x, y |]
        |> Array.filter (
            locate ocean
            >> ((energy >> (>) <.> 9) <&> (not << flashed))
        )
        |> Set.ofArray

    let rec flashing (toBeFlashed: (int * int) Set) ocean =
        match toBeFlashed with
        | EmptySet -> ocean
        | toBeFlashed ->
            let neighbors = toBeFlashed |> Set.map neighbors |> Set.toArray |> Array.concat
            let ocean =
                ocean
                |> Array2D.mapi (fun y x (e, f) ->
                                    if toBeFlashed |> Set.contains (x, y) then e, true
                                    else
                                        let energy = neighbors
                                                    |> Array.filter ((=) (x, y)) |> Array.length
                                        e + energy, f)
            flashing (waitingToFlash ocean) ocean

    let ocean = ocean |> Array2D.map (fun (e, f) -> e + 1, f)

    flashing (waitingToFlash ocean) ocean
    |> Array2D.map (fun (e, _) -> if e > 9 then 0, false else e, false)

let countFlash ocean =
    [| for x in 0 .. X do
        for y in 0 .. Y -> x, y |]
    |> Array.filter (locate ocean >> fst >> (=) 0)
    |> Array.length

let firsrPart ocean =
    [| 1 .. 100 |]
    |> Array.fold (fun (c, o) _ -> (c + countFlash o, flash o)) (0, flash ocean)
    |> fst

let secondPart ocean =
    let rec loop turn ocean =
        let count = countFlash ocean
        if count = (X + 1) * (Y + 1) then turn
        else loop (turn + 1) (flash ocean)

    loop 1 (flash ocean)

let input =
    File.ReadAllLines "input.txt"
    |> Array.map (Seq.toArray >> Array.map (string >> int))
    |> Array.map (fun row -> row |> Array.map (fun cell -> (cell, false)))
    |> array2D

input |> firsrPart |> printfn "First part: %d"
input |> secondPart |> printfn "Second part: %d"
