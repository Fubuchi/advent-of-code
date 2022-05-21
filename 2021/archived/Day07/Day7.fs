module Day7

open System.IO

let progression a b =
    (1 + (a - b |> abs)) * (a - b |> abs) / 2

let (|=>) crabs target fuel =
    crabs |> Array.fold (fuel target) 0

let firstPart target fuel (pos, count) =
    fuel + (pos - target |> abs) * count

let secondPart target fuel (pos, count) =
    fuel + (progression pos target) * count

let raw =
    (File.ReadAllLines "input.txt"
    |> Array.head).Split [| ',' |]
    |> Array.map int

let crabs = raw |> Array.countBy id
let positions = [| raw |> Array.min..raw |> Array.max |]

positions
    |> Array.map (fun pos -> (crabs |=> pos) firstPart)
    |> Array.minBy id
    |> printfn "First part: %d"

positions
    |> Array.map (fun pos -> (crabs |=> pos) secondPart)
    |> Array.minBy id
    |> printfn "Second part: %d"
