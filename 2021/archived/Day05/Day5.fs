module Day5

open System
open System.IO

let t2 a =
    match a with
    | [| x; y |] -> (x, y)
    | _ -> failwithf "Bang %A" a

let step a b = if a < b then 1 else -1

let straight (x1, y1) (x2, y2) =
    [| for x in x1 .. step x1 x2 .. x2 do
           for y in y1 .. step y1 y2 .. y2 -> (x, y) |]

let diagonal (x1, y1) (x2, y2) =
    Array.zip [| x1 .. step x1 x2 .. x2 |] [| y1 .. step y1 y2 .. y2 |]

let fisrstPart ((x1, y1), (x2, y2)) =
    if x1 <> x2 && y1 <> y2 then
        [||]
    else
        straight (x1, y1) (x2, y2)

let secondPart ((x1, y1), (x2, y2)) =
    if x1 <> x2 && y1 <> y2 then
        diagonal (x1, y1) (x2, y2)
    else
        straight (x1, y1) (x2, y2)


let solve input solution =
    input
    |> Array.map solution
    |> Array.concat
    |> Array.countBy id
    |> Array.filter (fun (_, count) -> count > 1)
    |> Array.length

let input =
    File.ReadAllLines "input.txt"
    |> Array.map (fun s -> s.Split([| " -> " |], StringSplitOptions.None))
    |> Array.map (fun pos ->
        let parse = (Array.map int) >> t2

        (pos.[0].Split [| ',' |] |> parse, pos.[1].Split [| ',' |] |> parse))

solve input fisrstPart |> printfn "First part: %d"
solve input secondPart |> printfn "First part: %d"
