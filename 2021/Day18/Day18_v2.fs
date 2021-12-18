module Day18

open System
open System.IO

let (<.>) f x y = f y x

let depth = snd
let value = fst

let extractNumber s =
    let rec loop s idx =
        match idx with
        | i when i = (s |> String.length) -> i - 1
        | i ->
            match s.[i] with
            | n when Char.IsDigit n -> loop s (i + 1)
            | _ -> i - 1

    loop s 0

let parse (s: string) =
    let rec loop s numbers depth =
        match s with
        | "" -> numbers |> List.rev |> List.toArray
        | s ->
            match s.[0] with
            | '[' -> loop s.[1..] numbers (depth + 1)
            | ']' -> loop s.[1..] numbers (depth - 1)
            | c when Char.IsDigit c ->
                let i = extractNumber s
                let n = s.[0..i] |> int
                loop s[i + 1..] ((n, depth) :: numbers) depth
            | _ -> loop s.[1..] numbers depth

    loop s [] 0

let checkExplode numbers = numbers |> Array.tryFindIndex (depth >> ((>=) <.> 5))

let checkSplit numbers = numbers |> Array.tryFindIndex (value >> ((>=) <.> 10))

let rec reduce numbers =
    match checkExplode numbers, checkSplit numbers with
    | Some (e), _ ->
        let limit = numbers |> Array.length |> ((-) <.> 2)
        let (lv, ld), (rv, rd) = numbers.[e], numbers.[e + 1]
        match e with
        | 0 ->
            let (v, d) = numbers.[2]
            reduce (Array.append [| (0, ld - 1); (v + rv, d) |] numbers.[3..])
        | i when i = limit ->
            let (v, d) = numbers.[limit - 1]
            reduce (Array.append numbers.[..limit - 2] [| (v + lv, d); (0, rd - 1) |])
        | i ->
            let (vl, dl), (vr, dr) = numbers.[i - 1], numbers.[i + 2]
            let left = Array.append numbers.[..i - 2] [| (vl + lv, dl); (0, ld - 1) |]
            let right = Array.append [| (vr + rv, dr) |] numbers.[i + 3..]
            reduce (Array.append left right)
    | _, Some(s) ->
        let (v, d) = numbers.[s]
        let left =  Array.append numbers.[..s - 1] [| (v / 2, d + 1); ((v + 1) / 2, d + 1) |]
        let right = numbers.[s + 1..]
        reduce (Array.append left right)
    | _ ->
        numbers

let sum left right =
    Array.append left right
    |> Array.map (fun (v, d) -> (v, d + 1))
    |> reduce

let rec total reduced =
    match reduced with
    | [| v |] -> v
    | xs ->
        let maxDepth = xs |> Array.map depth |> Array.max
        let i = xs |> Array.findIndex (fun (_, d) -> d = maxDepth)
        let left = xs.[..i - 1]
        let right = xs.[i + 2..]
        let (l, _), (r, _) = xs.[i], xs.[i + 1]
        total (right |> Array.append [| (3 * l + 2 * r, maxDepth - 1) |] |> Array.append left)

let firstPart numbers =
    numbers
    |> Array.reduce sum
    |> total
    |> value

let secondPart numbers =
    let limit = numbers |> Array.length |> ((-) <.> 1)
    Seq.allPairs [0..limit] [0..limit]
    |> Seq.filter (fun (x, y) -> x <> y)
    |> Seq.map (fun (x, y) -> firstPart [| numbers.[x]; numbers.[y] |])
    |> Seq.max

let math = File.ReadAllLines "input.txt"
            |> Array.map parse

firstPart math |> printfn "First part: %d"
secondPart math |> printfn "Second part: %d"
