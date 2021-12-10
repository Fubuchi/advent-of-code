module Day2

open System.IO

type Direction =
    | Forward of int
    | Down of int
    | Up of int

let parse (text: string) =
    match text.Split [| ' ' |] with
    | [| "forward"; v |] -> Forward(v |> int)
    | [| "down"; v |] -> Down(v |> int)
    | [| "up"; v |] -> Up(v |> int)
    | _ -> failwith $"Bad input: {text}"

let dive (hor, dep) curr =
    match curr with
    | Forward f -> (hor + f, dep)
    | Down d -> (hor, dep + d)
    | Up u -> (hor, dep - u)

let firstPart input = input |> Array.fold dive (0, 0)

let diveAim (hor, dep, aim) curr =
    match curr with
    | Forward f -> (hor + f, dep + f * aim, aim)
    | Down d -> (hor, dep, aim + d)
    | Up u -> (hor, dep, aim - u)

let secondPart input = input |> Array.fold diveAim (0, 0, 0)

let input =
    File.ReadAllLines "input.txt" |> Array.map parse

let (h1, d1) = firstPart input
let (h2, d2, _) = secondPart input

printfn $"First Part {h1 * d1}"
printfn $"Second Part {h2 * d2}"
