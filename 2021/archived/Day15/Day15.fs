module Day15

open System
open System.IO
open FSharpx.Collections

module PQueue = PriorityQueue
module HashMap = PersistentHashMap

let (<.>) f x y = f y x

let inBound X Y (x, y) =
    not <| (x < 1 || x > X || y < 1 || y > Y)

let neighbors X Y (x, y) =
    [| (0, 1); (0, -1); (1, 0); (-1, 0) |]
    |> Array.map (fun (vx, vy) -> x + vx, y + vy)
    |> Array.filter (inBound X Y)
    |> Set.ofArray

let risk X Y x y (tableX1: int array array) =
    if x <= X && y <= Y then
        tableX1.[y - 1].[x - 1]
    else
        let modX, modY = x % X, y % Y
        let quotientX, quotientY = x / X, y / Y
        let baseX = if modX = 0 then X else modX
        let baseY = if modY = 0 then Y else modY
        let baseEdge = tableX1.[baseY - 1].[baseX - 1]

        let jumpX =
            match modX, quotientX with
            | 0, q -> q - 1
            | _, q -> q

        let jumpY =
            match modY, quotientY with
            | 0, q -> q - 1
            | _, q -> q

        match (baseEdge + jumpX + jumpY) % 9 with
        | 0 -> 9
        | e -> e

let dijkstra X Y risks source destination =
    let evaluate evaluated costs node queue =
        let fromCost = costs |> HashMap.find node

        let toBeGuessed =
            neighbors X Y node
            |> Set.filter (not << (Set.contains <.> evaluated))

        toBeGuessed
        |> Set.fold
            (fun (costs, queue) node ->
                let currentCost = HashMap.find node costs
                let risk = HashMap.find node risks

                if risk + fromCost < currentCost then
                    costs |> HashMap.add node (risk + fromCost), PQueue.insert (risk + fromCost, node) queue
                else
                    costs, queue)
            (costs, queue)

    let rec checkCost costs evaluating evaluated =
        match evaluating, evaluated |> Set.contains destination with
        | e, _ when PQueue.isEmpty e -> costs
        | _, true -> costs
        | eval, _ ->
            let ((_, min), rest) = PQueue.pop eval
            let (costs, queue) = evaluate evaluated costs min rest
            checkCost costs queue (Set.add min evaluated)

    let costs =
        risks
        |> HashMap.map (fun _ -> Int32.MaxValue)
        |> HashMap.add (1, 1) 0

    checkCost costs (PQueue.empty false |> PQueue.insert (0, source)) Set.empty

let table =
    File.ReadAllLines "input.txt"
    |> Array.map (fun s -> s |> Seq.toArray |> Array.map (string >> int))

let Y = table |> Array.length

let X = table |> Array.head |> Array.length

let X5 = X * 5
let Y5 = Y * 5

let risks =
    [| for x in 1 .. X do
           for y in 1 .. Y -> (x, y), table.[y - 1].[x - 1] |]
    |> HashMap.ofSeq

let risksX5 =
    [| for x in 1 .. X5 do
           for y in 1 .. Y5 -> (x, y), risk X Y x y table |]
    |> HashMap.ofSeq

dijkstra X Y risks (1, 1) (X, Y)
|> HashMap.find (X, Y)
|> printfn "First part: %A"

dijkstra X5 Y5 risksX5 (1, 1) (X5, Y5)
|> HashMap.find (X5, Y5)
|> printfn "Second part: %A"
