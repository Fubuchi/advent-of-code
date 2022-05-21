module Day9

open System.IO

let (<.>) f x y = f y x

let (|.|) (t: 'a array array) (x, y) = t.[y].[x]

let (<+>) e s = Set.add e s

let X table = table |> Array.head |> Array.length |> ((-) <.> 1)

let Y table = table |> Array.length |> ((-) <.> 1)

let neighbors X Y cell =
    let move (x, y) (vx, vy) = x + vx, y + vy
    let inBound (x, y) = not <| (x < 0 || x > X || y < 0 || y > Y)

    [| (0, 1); (0, -1); (1, 0); (-1, 0) |]
    |> Array.map (move cell)
    |> Array.filter inBound

let isLow table cell =
    let X = X table
    let Y = Y table
    let current = table|.|cell

    neighbors X Y cell
    |> Array.forall (fun cell -> table|.|cell > current)

let lowest table =
    let X = X table
    let Y = Y table

    [| for x in 0..X do
        for y in 0..Y -> (x, y) |]
    |> Array.filter (isLow table)

let firstPart table =
    lowest table
    |> Array.map (fun cell -> table|.|cell |> (+) 1)
    |> Array.sum

let basin table cell =
    let X = X table
    let Y = Y table

    let rec loop visted cell =
        if Set.contains cell visted || table|.|cell = 9 then
            visted
        else
            neighbors X Y cell
            |> Array.fold loop (cell <+> visted)

    loop Set.empty cell |> Set.count

let secondPart table =
    lowest table
    |> Array.map (basin table)
    |> Array.sortDescending
    |> Array.take 3
    |> Array.reduce (*)

let input = File.ReadAllLines "input.txt"
            |> Array.map Seq.toArray
            |> Array.map (Array.map (string >> int))

firstPart input |> printfn "First part: %d"
secondPart input |> printfn "Second part: %d"
