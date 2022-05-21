module Day13

open System.IO

let fold paper instructions =
    instructions
    |> Array.fold
        (fun p (target, source) ->
            if p |> Set.contains source then
                p |> Set.remove source |> Set.add target
            else
                p)
        paper

let foldX Y col paper =
    [| for x in col + 1 .. col * 2 do
           for y in 0 .. Y -> ((x - 2 * (x - col), y), (x, y)) |]
    |> fold paper

let foldY X line paper =
    [| for x in 0 .. X do
           for y in line + 1 .. line * 2 -> ((x, y - 2 * (y - line)), (x, y)) |]
    |> fold paper

let firstPart X Y instruction paper =
    match instruction with
    | ("x", col) -> foldX Y col paper |> Set.count
    | ("y", line) -> foldY X line paper |> Set.count
    | oops -> failwithf "Unkown instruction %A" oops

let secondPart X Y instructions paper =
    let result =
        instructions
        |> Array.fold
            (fun p ins ->
                match ins with
                | ("x", col) -> foldX Y col p
                | ("y", line) -> foldY X line p
                | oops -> failwithf "Unkown instruction %A" oops)
            paper

    let left = result |> Seq.minBy fst |> fst
    let right = result |> Seq.maxBy fst |> fst

    let up = result |> Seq.minBy snd |> snd
    let down = result |> Seq.maxBy snd |> snd

    [| for y in up .. down do
           [| for x in left .. right ->
                  if result |> Set.contains (x, y) then
                      "0"
                  else
                      " " |] |]

let paper =
    File.ReadAllLines "paper.txt"
    |> Array.map (fun s ->
        let ins = s.Split(",")
        (ins.[0] |> int, ins.[1] |> int))
    |> Set.ofArray

let instructions =
    File.ReadAllLines "fold.txt"
    |> Array.map (fun s ->
        let ins = s.Split("=")
        (ins.[0], ins.[1] |> int))

let X = paper |> Seq.maxBy fst |> fst
let Y = paper |> Seq.maxBy snd |> snd

firstPart X Y instructions.[0] paper
|> printfn "First part: %d"

printfn "Second part:"

secondPart X Y instructions paper
|> Array.map (String.concat "")
|> Array.iter (printfn "%s")
