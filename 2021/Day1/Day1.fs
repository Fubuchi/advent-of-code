module Day1

open System.IO

let input = File.ReadAllLines "input.txt" |> Array.map int

let deeper count (prev, curr) =
    if curr > prev then count + 1
    else count

let fisrtPart input =
    input
        |> Array.pairwise
        |> Array.fold deeper 0

let secondPart input =
    input
        |> Array.windowed 3
        |> Array.map Array.sum
        |> Array.pairwise
        |> Array.fold deeper 0

fisrtPart input |> printfn "First %d"
secondPart input |> printfn "Second %d"
